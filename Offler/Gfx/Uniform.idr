||| The two uniform scratch buffers, and the only things allowed to write into
||| them.
|||
||| `Offler.Gfx.Layout` says where each field goes; this says who may put one
||| there. The WebGPU-flavoured backends would otherwise each repeat the same
||| pokes per object and per frame, each with its own copy of the offsets and
||| its own `i >= maxObjects` test at the top. Multiple copies of a bounds
||| check are multiple chances to omit one -- and omitting one writes into the
||| next object's slot, which shows up as a wrong colour rather than an error.
module Offler.Gfx.Uniform

import Offler.Camera
import Offler.Color
import Offler.Gfx.Array
import Offler.Gfx.Config
import Offler.Gfx.Layout
import Offler.Light
import Offler.Material
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

||| One slot per object, filled during the frame and uploaded in a single
||| write at the end. Writing per object means one queue call per draw, which
||| is what made the orrery's WebGPU path far slower than WebGL2 at scale.
public export
objScratchFloats : Int
objScratchFloats = objFloats * maxObjects

public export
ObjScratch : Type
ObjScratch = F32Array Offler.Gfx.Uniform.objScratchFloats

export
newObjScratch : IO ObjScratch
newObjScratch = newF32 objScratchFloats

||| An object's place in the uniform buffer: a window that has been checked.
|||
||| `MkSlot` is private and every draw takes a `Slot` rather than an `Int`, so
||| the check cannot be skipped, cannot be written differently in two
||| backends, and cannot be made against the wrong bound.
|||
||| One constructor and one field, so the newtype optimisation leaves nothing
||| of it at run time. The index and the byte offset are recovered from the
||| offset rather than stored beside it, which would be a second allocation
||| per object per frame.
export
data Slot : Type where
  MkSlot : At Offler.Gfx.Uniform.objScratchFloats Offler.Gfx.Layout.objFloats -> Slot

||| One test, covering both uses of the index. The scratch holds exactly
||| `objFloats * maxObjects` floats and the uniform buffer exactly
||| `objStride * maxObjects` bytes, and `objFloats * 4 = objStride` is proved
||| in `Offler.Gfx.Layout` -- so a window that fits the scratch is a dynamic
||| offset the bind group will also accept, and there is no second bound to
||| check.
|||
||| `Nothing` means the frame has run out of slots. The caller must decide
||| what that means; it can no longer be forgotten.
export
slot : ObjScratch -> (i : Int) -> Maybe Slot
slot a i = case window {w = objFloats} a (i * objFloats) of
             Just o => Just (MkSlot o)
             Nothing => Nothing

||| The slot number. The C shim scales it by `objStride` itself, which is why
||| it wants this rather than the byte offset.
export %inline
slotIndex : Slot -> Int
slotIndex (MkSlot o) = atOffset o `div` objFloats

||| The dynamic bind-group offset, in bytes. `index * objStride` and
||| `atOffset * 4` are the same number because `objFloats * 4 = objStride`,
||| which `Offler.Gfx.Layout.objFloatsOk` proves -- so this needs no division.
export %inline
slotOffset : Slot -> Int
slotOffset (MkSlot o) = atOffset o * 4

||| Write one object's uniform record. The offsets inside the record are the
||| layout's business and this is the only thing that knows them.
export
pokeObject : ObjScratch -> Slot -> Mat4 -> Material -> IO ()
pokeObject a (MkSlot o) model mat = do
  pokeMat a (sub 0 o) model
  poke4 a (sub objBaseColorFloat o)
        mat.baseColor.red mat.baseColor.green mat.baseColor.blue mat.baseColor.alpha
  poke4 a (sub objEmissiveFloat o)
        mat.emissive.red mat.emissive.green mat.emissive.blue (modeCode mat)
  poke4 a (sub objParamsFloat o) mat.metallic mat.roughness 0.0 0.0

||| A slot asserted rather than tested, private to this module: what the
||| batch loop below steps with once its bound is dealt with. `slot` remains
||| the only way in from outside.
slotAt : Int -> Slot
slotAt i = MkSlot (unsafeAt (i * objFloats))

||| The batched form of `pokeObject`: fill consecutive slots from `first`,
||| returning the next free index. This is the per-object hot path, so the
||| §8.1 lesson from the orrery's notes applies -- the measured cost of the
||| checked constructor was the `Maybe` it allocates, not the comparison --
||| and the loop therefore tests a bare `i < maxObjects` per object instead
||| of allocating a `Just` per object. The test still exists exactly once,
||| inside the module that owns the bound; what moved is its result from a
||| heap value to a branch.
|||
||| `boundsMode` is a compile-time constant from the generated
||| `Offler.Gfx.Config`, so the branch between the two loops is decided
||| before either runs: `make BOUNDS=trusted` removes even the comparison.
||| Trusted is only sane when the application bounds its own draw count --
||| past the end, the JS backend silently drops the writes and the C side
||| corrupts the heap.
-- Written in `PrimIO` with the world bound on the left-hand side,
-- deliberately: with the world inside an `IO` do-block the JS backend
-- compiled the recursion as a call returning a world-lambda that was then
-- applied -- not a self tail call, so no `__tailRec`, and the stack
-- overflowed V8 at exactly `maxObjects`. With the world as an ordinary
-- argument the recursive call is saturated and the trampoline fires. Found
-- the way §8d.7 of the orrery's notes says such things are found: a node
-- program that runs the loop at the cap, and a grep of the generated
-- output. Neither problem nor fix is visible in this source.
goChecked : ObjScratch -> Int -> List (Mat4, Material) -> PrimIO Int
goChecked a i [] w = MkIORes i w
goChecked a i ((m, mt) :: rest) w =
  if i >= maxObjects
    then MkIORes i w
    else case toPrim (pokeObject a (slotAt i) m mt) w of
           MkIORes _ w' => goChecked a (i + 1) rest w'

goTrusted : ObjScratch -> Int -> List (Mat4, Material) -> PrimIO Int
goTrusted a i [] w = MkIORes i w
goTrusted a i ((m, mt) :: rest) w =
  case toPrim (pokeObject a (slotAt i) m mt) w of
    MkIORes _ w' => goTrusted a (i + 1) rest w'

export
pokeObjects : ObjScratch -> (first : Int) -> List (Mat4, Material) -> IO Int
pokeObjects a first batch =
  case boundsMode of
    Checked => fromPrim (goChecked a (max 0 first) batch)
    Trusted => fromPrim (goTrusted a (max 0 first) batch)

||| Write the frame globals. Also where the clip-space correction is applied,
||| which both WebGPU-flavoured backends need and neither should have to
||| remember. (WebGL2 does not use this scratch at all: it sets classic
||| uniforms one by one, in [-1,1] clip space.)
|||
||| Every offset here is a literal against a known capacity, so all the
||| bounds are discharged at compile time and the last `poke4`, at 40, is
||| exactly flush with the end. Add a field to `globalFields` and this stops
||| compiling rather than running off the end of the buffer.
export
pokeGlobals : GlobalScratch -> Camera -> (aspectRatio : Double) -> Lights
            -> (time : Double) -> IO ()
pokeGlobals a cam aspectRatio lights t = do
  pokeMat a (here 0) (projMatrix cam.projection aspectRatio)
  correctClipZ a (here 0)
  pokeMat a (here 16) (viewMatrix cam)
  let eye = eyeOf cam
      dir = normalize3 lights.direction
      lc = lights.color
  poke4 a (here 32) eye.vx eye.vy eye.vz t
  poke4 a (here 36) dir.vx dir.vy dir.vz lights.ambient
  poke4 a (here 40) lc.red lc.green lc.blue 0.0
