||| `Offler.Audio` over Web Audio. The one browser-specific wrinkle is the
||| autoplay policy: an `AudioContext` is born suspended until the page sees
||| a real user gesture, so `initAudio` installs one-shot resume listeners
||| and early plays land on a suspended context -- started, silent, and
||| harmless -- rather than being errors.
|||
||| Voices are slots in a runtime array with generations, recycled by each
||| source's `onended`; the same free-list-plus-generation scheme as the
||| renderer's meshes, so a stale `Voice` controls nothing.
module Offler.Web.Audio

import public Offler.Audio
import Offler.Web.Js

%default covering

%foreign "javascript:lambda:()=>{const ctx=new (window.AudioContext||window.webkitAudioContext)();const master=ctx.createGain();master.connect(ctx.destination);const resume=()=>{if(ctx.state==='suspended')ctx.resume()};addEventListener('pointerdown',resume);addEventListener('keydown',resume);addEventListener('touchstart',resume);return {ctx:ctx,master:master,sounds:[],voices:[],free:[]}}"
prim__initAudio : PrimIO JSVal

%foreign "javascript:lambda:(rt,rate,data,n)=>{const b=rt.ctx.createBuffer(1,Math.max(1,n),rate);b.copyToChannel(data,0);return rt.sounds.push({b:b})-1}"
prim__loadPcm : JSVal -> Int -> AnyPtr -> Int -> PrimIO Int

||| The handle exists at once; the buffer arrives when the decode does.
%foreign "javascript:lambda:(rt,url)=>{const i=rt.sounds.push({b:null})-1;fetch(url).then(r=>r.arrayBuffer()).then(a=>rt.ctx.decodeAudioData(a)).then(b=>{rt.sounds[i].b=b}).catch(e=>console.warn('offler audio: '+url,e));return i}"
prim__loadUrl : JSVal -> String -> PrimIO Int

||| A fresh node graph per play (source -> gain -> pan -> master): sources
||| are one-shot in Web Audio, and the nodes are what setVoice adjusts.
||| Returns the voice slot, or -1 when the sound has no buffer yet.
%foreign "javascript:lambda:(rt,sid,gain,pan,rate,loop)=>{const s=rt.sounds[sid];if(!s||!s.b)return -1;let vi=rt.free.pop();if(vi===undefined){vi=rt.voices.push({gen:0})-1}const V=rt.voices[vi];const src=rt.ctx.createBufferSource();src.buffer=s.b;src.playbackRate.value=rate;src.loop=!!loop;const g=rt.ctx.createGain();g.gain.value=gain;const p=rt.ctx.createStereoPanner();p.pan.value=Math.max(-1,Math.min(1,pan));src.connect(g);g.connect(p);p.connect(rt.master);const gen=V.gen;V.src=src;V.g=g;V.p=p;V.live=true;src.onended=()=>{if(V.gen===gen&&V.live){V.live=false;V.gen++;rt.free.push(vi)}};src.start();return vi}"
prim__play : JSVal -> Int -> Double -> Double -> Double -> Int -> PrimIO Int

%foreign "javascript:lambda:(rt,vi)=>{const V=rt.voices[vi];return V?V.gen:0}"
prim__voiceGen : JSVal -> Int -> PrimIO Int

%foreign "javascript:lambda:(rt,vi,gen,gain,pan)=>{const V=rt.voices[vi];if(!V||!V.live||V.gen!==gen)return 0;V.g.gain.value=gain;V.p.pan.value=Math.max(-1,Math.min(1,pan));return 0}"
prim__setVoice : JSVal -> Int -> Int -> Double -> Double -> PrimIO ()

%foreign "javascript:lambda:(rt,vi,gen)=>{const V=rt.voices[vi];if(!V||!V.live||V.gen!==gen)return 0;try{V.src.stop()}catch(e){}return 0}"
prim__stopVoice : JSVal -> Int -> Int -> PrimIO ()

%foreign "javascript:lambda:(rt,g)=>{rt.master.gain.value=g;return 0}"
prim__setMaster : JSVal -> Double -> PrimIO ()

public export
record WebAudio where
  constructor MkWebAudio
  rt : JSVal

export
initAudio : IO WebAudio
initAudio = MkWebAudio <$> primIO prim__initAudio

export
Audio WebAudio where
  loadSound a (SoundPcm rate xs) = do
    (p, n) <- stagePcm xs
    soundHandle <$> primIO (prim__loadPcm a.rt rate p n)
  loadSound a (SoundFile url) =
    soundHandle <$> primIO (prim__loadUrl a.rt url)

  play a h pb = do
    vi <- primIO (prim__play a.rt (soundIndex h)
                    pb.gain pb.pan pb.rate (if pb.loop then 1 else 0))
    if vi < 0
      then pure noVoice
      else voice vi <$> primIO (prim__voiceGen a.rt vi)

  setVoice a v g p = primIO (prim__setVoice a.rt (voiceIndex v) (voiceGen v) g p)

  stopVoice a v = primIO (prim__stopVoice a.rt (voiceIndex v) (voiceGen v))

  setMasterGain a g = primIO (prim__setMaster a.rt g)
