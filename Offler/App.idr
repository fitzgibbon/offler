||| The application-lifetime seam: an `App` is a whole program minus its
||| backends. A scene provides one polymorphic `scene` function; a backend
||| launcher (`Offler.Native.App`, `Offler.Web.App`) constructs the
||| renderer, platform and audio and hands them in. The rank-2 field is the
||| guarantee: an `App` cannot mention anything backend-specific, so the
||| same value launches on SDL3, WebGL2 and WebGPU.
module Offler.App

import public Offler.Audio
import public Offler.Gfx.Platform
import public Offler.Gfx.Renderer

||| What a scene shows in its page footer or window title -- chrome, not a
||| `Platform` concept, which is why it rides beside the capabilities
||| rather than inside one.
public export
Status : Type
Status = String -> String -> IO ()

public export
record App where
  constructor MkApp
  ||| The window title; launchers also derive console messages from it.
  title : String
  scene : {0 r, f, p, au : Type} ->
          Renderer r f => Platform p => Audio au =>
          r -> p -> au -> Status -> IO ()

||| Wrap a scene that plays no sound. The audio backend is still
||| constructed -- the lifetime is uniform -- it is merely never spoken to.
public export
soundless : ({0 r, f, p : Type} -> Renderer r f => Platform p =>
             r -> p -> Status -> IO ())
         -> {0 r, f, p, au : Type} ->
            Renderer r f => Platform p => Audio au =>
            r -> p -> au -> Status -> IO ()
soundless f r p _ status = f r p status
