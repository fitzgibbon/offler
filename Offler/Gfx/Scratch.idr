||| The one big capacity, behind a module boundary that keeps it abstract.
|||
||| `objScratchFloats` is 655 360 -- and a `Nat` that large is poison to the
||| elaborator if it can see the value: conversion checking normalizes `Nat`s
||| to successor form one `S` at a time (~10s per forcing at this size, ~70s
||| inside a lifted `case` block, linear in the value), which is what made
||| `Offler.Gfx.Uniform` appear to hang. The fix is `export` without
||| `public`: outside this module the body is invisible to conversion, so
||| the name is an abstract constant compared syntactically -- measured at
||| 0.3s where the transparent spelling took 70.
|||
||| The price of opacity is that nothing outside can *reduce* it, so the
||| facts others need are proved here, where the body is visible, and
||| exported as erased equations. Small constants (`objFloatsN`,
||| `globalFloatsN`) stay `public export` in `Offler.Gfx.Layout`: proof
||| search needs their values, and normalizing 64 or 72 is free.
module Offler.Gfx.Scratch

import Offler.Gfx.Layout

%default total

||| Floats in one object scratch: `maxObjects` slots of `objFloats` floats.
||| Abstract outside this module -- see the module comment.
export
objScratchFloats : Nat
objScratchFloats = 655360

||| The tie to the layout's `Int` constants, proved where the body is
||| visible. This single equation is the module's one slow elaboration
||| (~4s): it forces the literal through `integerToNat` once.
export
0 objScratchFloatsOk : cast Offler.Gfx.Scratch.objScratchFloats
                     = Offler.Gfx.Layout.objFloats * Offler.Gfx.Layout.maxObjects
objScratchFloatsOk = Refl
