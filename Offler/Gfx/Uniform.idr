||| The uniform scratch buffers, and the only things allowed to write into
||| them.
|||
||| `Offler.Gfx.Layout` says where each engine field goes; this says who may
||| put one there. Three buffers cross to the GPU: the globals each frame,
||| the per-draw engine blocks (model matrix and alpha lane) each frame, and
||| the per-*asset* material blocks -- written through the `MatWriter` a
||| material's `writeMat` receives when the asset is added or updated, not
||| per draw. A draw pairs an object slot with its material's asset slot
||| through two independent dynamic offsets.
module Offler.Gfx.Uniform

import Data.IORef
import Data.List
import Offler.Camera
import Offler.Color
import Offler.Gfx.Array
import Offler.Gfx.Config
import Offler.Gfx.Layout
import Offler.Light
import Offler.Math

%default total

||| Floats in the frame-global scratch: exactly the block `Offler.Gfx.Layout`
||| lays out.
public export
GlobalScratch : Type
GlobalScratch = F32Array Offler.Gfx.Layout.globalFloats

export
newGlobalScratch : IO GlobalScratch
newGlobalScratch = newF32 globalFloats

||| One slot per draw, filled during the frame and uploaded in a single
||| write at the end. Writing per draw means one queue call each, which is
||| what made the orrery's WebGPU path far slower than WebGL2 at scale. The
||| same type serves the object buffer and the material buffer: both are
||| `maxObjects` slots of `objStride` bytes.
public export
objScratchFloats : Int
objScratchFloats = objFloats * maxObjects

public export
ObjScratch : Type
ObjScratch = F32Array Offler.Gfx.Uniform.objScratchFloats

export
newObjScratch : IO ObjScratch
newObjScratch = newF32 objScratchFloats

||| A draw's place in the uniform buffers: a window that has been checked.
|||
||| `MkSlot` is private and every draw takes a `Slot` rather than an `Int`,
||| so the check cannot be skipped, cannot be written differently in two
||| backends, and cannot be made against the wrong bound.
export
data Slot : Type where
  MkSlot : At Offler.Gfx.Uniform.objScratchFloats Offler.Gfx.Layout.objFloats -> Slot

||| One test, covering every use of the index. The scratches hold exactly
||| `objFloats * maxObjects` floats and the uniform buffers exactly
||| `objStride * maxObjects` bytes, and `objFloats * 4 = objStride` is
||| proved in `Offler.Gfx.Layout` -- so a window that fits the scratch is a
||| dynamic offset the bind group will also accept.
export
slot : ObjScratch -> (i : Int) -> Maybe Slot
slot a i = case window {w = objFloats} a (i * objFloats) of
             Just o => Just (MkSlot o)
             Nothing => Nothing

||| The slot number. The C shim scales it by `objStride` itself, which is
||| why it wants this rather than the byte offset.
export %inline
slotIndex : Slot -> Int
slotIndex (MkSlot o) = atOffset o `div` objFloats

||| The dynamic bind-group offset, in bytes. `index * objStride` and
||| `atOffset * 4` are the same number because `objFloats * 4 = objStride`,
||| which `Offler.Gfx.Layout.objFloatsOk` proves -- so this needs no
||| division.
export %inline
slotOffset : Slot -> Int
slotOffset (MkSlot o) = atOffset o * 4

||| Write a draw's engine block: the model matrix and the four lane floats
||| (alpha mode and cutoff for material draws; an RGBA colour for the line
||| pipeline, which reuses the block). The offsets are the layout's business
||| and this is the only thing that knows them.
export
pokeObject : ObjScratch -> Slot -> Mat4
          -> (laneX, laneY, laneZ, laneW : Double) -> IO ()
pokeObject a (MkSlot o) model x y z w = do
  pokeMat a (sub 0 o) model
  poke4 a (sub objLaneFloat o) x y z w

--------------------------------------------------------------------------------
-- Paged slots

||| Slots without a global ceiling: `maxObjects`-slot pages, grown on
||| demand, so `maxObjects` bounds a *page*, not a frame. The CPU pages
||| live here; each backend grows its GPU buffers (and, on the WebGPU
||| flavours, per-page bind groups) to match, lazily, keyed by the same
||| page arithmetic: global slot `i` is slot `i mod maxObjects` of page
||| `i div maxObjects`.
export
record Paged where
  constructor MkPaged
  pagesRef : IORef (List ObjScratch)

export
covering
newPaged : IO Paged
newPaged = do
  p0 <- newObjScratch
  ref <- newIORef [p0]
  pure (MkPaged ref)

covering
growTo : IORef (List ObjScratch) -> Int -> IO (List ObjScratch)
growTo ref n = do
  ps <- readIORef ref
  let have = the Int (cast (length ps))
  if have >= n
    then pure ps
    else do
      more <- mkMore (n - have)
      let ps' = ps ++ more
      writeIORef ref ps'
      pure ps'
  where
    covering
    mkMore : Int -> IO (List ObjScratch)
    mkMore k =
      if k <= 0 then pure []
      else do
        a <- newObjScratch
        rest <- mkMore (k - 1)
        pure (a :: rest)

nth : Int -> List a -> Maybe a
nth _ [] = Nothing
nth i (x :: xs) = if i <= 0 then Just x else nth (i - 1) xs

||| The page index, page scratch and checked local slot for global slot
||| `i`, growing the pages to reach it. The local index is `i mod
||| maxObjects`, so the `slot` test cannot fail for non-negative `i` --
||| the `Maybe` survives for negative input alone.
export
covering
pageSlot : Paged -> (i : Int) -> IO (Maybe (Int, ObjScratch, Slot))
pageSlot pg i =
  if i < 0 then pure Nothing
  else do
    let p = i `div` maxObjects
        local = i `mod` maxObjects
    ps <- growTo pg.pagesRef (p + 1)
    pure $ do
      a <- nth p ps
      s <- slot a local
      pure (p, a, s)

||| The pages holding slots `[0, count)`, each with the float prefix it
||| actually used -- what `endFrame` uploads, one write per touched page.
export
usedPages : Paged -> (count : Int) -> IO (List (Int, ObjScratch, Int))
usedPages pg count = do
  ps <- readIORef pg.pagesRef
  pure (go 0 count ps)
  where
    go : Int -> Int -> List ObjScratch -> List (Int, ObjScratch, Int)
    go _ _ [] = []
    go p remaining (a :: rest) =
      if remaining <= 0 then []
      else let n = min remaining maxObjects
            in (p, a, n * objFloats) :: go (p + 1) (remaining - n) rest

--------------------------------------------------------------------------------
-- The material writer

||| Where a material's `writeMat` may write: its own 256-byte slot of the
||| material buffer, addressed in 16-byte *lanes* -- lane `k` is bytes
||| `16k .. 16k+15` of the block, matching the uniform layout's alignment,
||| so `matFields` like `[baseColor Vec4, params Vec4]` sit at lanes 0 and 1.
|||
||| The constructor is private: a writer exists only for the slot a backend
||| is currently filling, so a material cannot write anywhere else. Each put
||| is one bare comparison against the slot's sixteen lanes -- no `Maybe`,
||| per the §8.1 measurement -- and an out-of-range lane is dropped.
export
data MatWriter : Type where
  MkMatWriter : ObjScratch -> (base : Int) -> MatWriter

||| For backends only: the writer for a slot of the material scratch.
export
matWriter : ObjScratch -> Slot -> MatWriter
matWriter a (MkSlot o) = MkMatWriter a (atOffset o)

||| Sixteen 4-float lanes per 256-byte slot.
lanesPerSlot : Int
lanesPerSlot = 16

export
putVec4 : MatWriter -> (lane : Int) -> (x, y, z, w : Double) -> IO ()
putVec4 (MkMatWriter a base) lane x y z w =
  when (lane >= 0 && lane < lanesPerSlot) $
    poke4 a (unsafeAt (base + lane * 4)) x y z w

export
putColor : MatWriter -> (lane : Int) -> Color -> IO ()
putColor w lane c = putVec4 w lane c.red c.green c.blue c.alpha

export
putMat4 : MatWriter -> (lane : Int) -> Mat4 -> IO ()
putMat4 (MkMatWriter a base) lane mat =
  when (lane >= 0 && lane + 3 < lanesPerSlot) $
    pokeMat a (unsafeAt (base + lane * 4)) mat

||| One float, at component `c` (0..3) of a lane.
export
putF : MatWriter -> (lane : Int) -> (c : Int) -> Double -> IO ()
putF (MkMatWriter a base) lane c v =
  when (lane >= 0 && lane < lanesPerSlot && c >= 0 && c <= 3) $
    poke a (unsafeAt (base + lane * 4 + c)) v

--------------------------------------------------------------------------------
-- Batch filling

-- Written in `PrimIO` with the world bound on the left-hand side,
-- deliberately: with the world inside an `IO` do-block the JS backend
-- compiled the recursion as a call returning a world-lambda that was then
-- applied -- not a self tail call, so no `__tailRec`, and the stack
-- overflowed V8 at exactly `maxObjects`. With the world as an ordinary
-- argument the recursive call is saturated and the trampoline fires.
-- `Checks/StackCheck.idr` holds this property in place.
goChecked : (x -> Slot -> IO ()) -> ObjScratch -> Int -> List x -> PrimIO Int
goChecked k a i [] w = MkIORes i w
goChecked k a i (v :: rest) w =
  if i >= maxObjects
    then MkIORes i w
    else case toPrim (k v (MkSlot (unsafeAt (i * objFloats)))) w of
           MkIORes _ w' => goChecked k a (i + 1) rest w'

goTrusted : (x -> Slot -> IO ()) -> ObjScratch -> Int -> List x -> PrimIO Int
goTrusted k a i [] w = MkIORes i w
goTrusted k a i (v :: rest) w =
  case toPrim (k v (MkSlot (unsafeAt (i * objFloats)))) w of
    MkIORes _ w' => goTrusted k a (i + 1) rest w'

||| Run a writer over consecutive slots from `first`, returning the next
||| free index: the per-draw hot path of every batched draw. The per-item
||| bound is a bare `i < maxObjects` comparison rather than a `Maybe` per
||| item -- the §8.1 measurement found the allocation, not the comparison,
||| was the cost -- and the test exists exactly once, here, inside the
||| module that owns the invariant. `boundsMode` is a compile-time constant
||| from the generated `Offler.Gfx.Config`, so the branch between the two
||| loops is decided before either runs: `make BOUNDS=trusted` removes even
||| the comparison, and is only sane when the application bounds its own
||| draw count.
export
fillWith : ObjScratch -> (first : Int) -> List x -> (x -> Slot -> IO ()) -> IO Int
fillWith a first batch k =
  case boundsMode of
    Checked => fromPrim (goChecked k a (max 0 first) batch)
    Trusted => fromPrim (goTrusted k a (max 0 first) batch)

--------------------------------------------------------------------------------
-- Globals

||| Write the frame globals. All three backends share this block byte for
||| byte -- std140 and WGSL lay these field types out identically -- and
||| differ only in `correctClip`: the WebGPU-flavoured backends pass `True`
||| to rewrite the projection's z row into [0,1] clip space, WebGL2 passes
||| `False` because GL wants [-1,1].
|||
||| The literal offsets are discharged at compile time; the per-light
||| offsets are computed from a bounded index, one `window` test each on a
||| once-per-frame path. The last colour lane sits exactly flush with the
||| end: grow `globalFields` and this stops compiling rather than running
||| off the buffer.
export
pokeGlobals : GlobalScratch -> Camera -> (aspectRatio : Double) -> Lights
            -> (time : Double) -> (correctClip : Bool) -> IO ()
pokeGlobals a cam aspectRatio lights t correct = do
  pokeMat a (here 0) (projMatrix cam.projection aspectRatio)
  when correct (correctClipZ a (here 0))
  pokeMat a (here 16) (viewMatrix cam)
  let eye = eyeOf cam
      ls = take (cast maxLights) lights.directionals
  poke4 a (here 32) eye.vx eye.vy eye.vz t
  poke4 a (here 36) (cast (length ls)) lights.ambient 0.0 0.0
  pokeLights 0 ls
  where
    pokeLights : Int -> List DirectionalLight -> IO ()
    pokeLights _ [] = pure ()
    pokeLights i (l :: rest) = do
      let d = normalize3 l.direction
          c = l.color
      case window {w = 4} a (lightDirsFloat + i * 4) of
        Just o => poke4 a o d.vx d.vy d.vz 0.0
        Nothing => pure ()
      case window {w = 4} a (lightColorsFloat + i * 4) of
        Just o => poke4 a o c.red c.green c.blue 0.0
        Nothing => pure ()
      pokeLights (i + 1) rest
