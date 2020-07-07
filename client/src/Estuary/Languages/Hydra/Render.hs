{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Languages.Hydra.Render (Hydra(..),newHydra,setResolution,evaluate,tick) where

import GHCJS.Types
import GHCJS.DOM.Types (HTMLCanvasElement)
import GHCJS.Marshal.Pure

import Estuary.Languages.Hydra.Types

newtype Hydra = Hydra JSVal

instance PToJSVal Hydra where pToJSVal (Hydra x) = x

instance PFromJSVal Hydra where pFromJSVal = Hydra

foreign import javascript safe
  "new Hydra({ canvas:$1, autoLoop:false, makeGlobal:false})"
  newHydra :: HTMLCanvasElement -> IO Hydra

foreign import javascript safe
  "$1.setResolution($2,$3);"
  setResolution :: Hydra -> Int -> Int -> IO ()

evaluate :: Hydra -> [Statement] -> IO ()
evaluate h = mapM_ (evaluateStatement h)

foreign import javascript safe
  "$1.tick($2);"
  tick :: Hydra -> Double -> IO () -- second argument is time in millis since last tick

sourceToJSVal :: Hydra -> Source -> JSVal
sourceToJSVal _ (ConstantInt x) = pToJSVal x
sourceToJSVal _ (ConstantDouble x) = pToJSVal x
sourceToJSVal _ (List []) = _list0 -- the method for list here is super-hacky and will be replaced...
sourceToJSVal h (List (x:[])) = _list1 (sourceToJSVal h x)
sourceToJSVal h (List (x:y:[])) = _list2 (sourceToJSVal h x) (sourceToJSVal h y)
sourceToJSVal h (List (x:y:z:_)) = _list3 (sourceToJSVal h x) (sourceToJSVal h y) (sourceToJSVal h z)
sourceToJSVal h (Osc []) = _osc0 h
sourceToJSVal h (Osc (x:[])) = _osc1 h (sourceToJSVal h x)
sourceToJSVal h (Osc (x:y:[])) = _osc2 h (sourceToJSVal h x) (sourceToJSVal h y)
sourceToJSVal h (Osc (x:y:z:_)) = _osc3 h (sourceToJSVal h x) (sourceToJSVal h y) (sourceToJSVal h z)
sourceToJSVal h (Solid []) = _solid0 h
sourceToJSVal h (Solid (x:[])) = _solid1 h (sourceToJSVal h x)
sourceToJSVal h (Solid (x:y:[])) = _solid2 h (sourceToJSVal h x) (sourceToJSVal h y)
sourceToJSVal h (Solid (x:y:z:[])) = _solid3 h (sourceToJSVal h x) (sourceToJSVal h y) (sourceToJSVal h z)
sourceToJSVal h (Solid (w:x:y:z:_)) = _solid4 h (sourceToJSVal h w) (sourceToJSVal h x) (sourceToJSVal h y) (sourceToJSVal h z)
sourceToJSVal h (Gradient []) = _gradient0 h
sourceToJSVal h (Gradient (x:_)) = _gradient1 h (sourceToJSVal h x)
sourceToJSVal h (Noise []) = _noise0 h
sourceToJSVal h (Noise (x:[])) = _noise1 h (sourceToJSVal h x)
sourceToJSVal h (Noise (x:y:_)) = _noise2 h (sourceToJSVal h x) (sourceToJSVal h y)
sourceToJSVal h (Shape []) = _shape0 h
sourceToJSVal h (Shape (x:[])) = _shape1 h (sourceToJSVal h x)
sourceToJSVal h (Shape (x:y:[])) = _shape2 h (sourceToJSVal h x) (sourceToJSVal h y)
sourceToJSVal h (Shape (x:y:z:_)) = _shape3 h (sourceToJSVal h x) (sourceToJSVal h y) (sourceToJSVal h z)
sourceToJSVal h (Voronoi []) = _voronoi0 h
sourceToJSVal h (Voronoi (x:[])) = _voronoi1 h (sourceToJSVal h x)
sourceToJSVal h (Voronoi (x:y:[])) = _voronoi2 h (sourceToJSVal h x) (sourceToJSVal h y)
sourceToJSVal h (Voronoi (x:y:z:_)) = _voronoi3 h (sourceToJSVal h x) (sourceToJSVal h y) (sourceToJSVal h z)

foreign import javascript safe "[]" _list0 :: JSVal
foreign import javascript safe "[$1]" _list1 :: JSVal -> JSVal
foreign import javascript safe "[$1,$2]" _list2 :: JSVal -> JSVal -> JSVal
foreign import javascript safe "[$1,$2,$3]" _list3 :: JSVal -> JSVal -> JSVal -> JSVal
foreign import javascript safe "$1.synth.osc()" _osc0 :: Hydra -> JSVal
foreign import javascript safe "$1.synth.osc($2)" _osc1 :: Hydra -> JSVal -> JSVal
foreign import javascript safe "$1.synth.osc($2,$3)" _osc2 :: Hydra -> JSVal -> JSVal -> JSVal
foreign import javascript safe "$1.synth.osc($2,$3,$4)" _osc3 :: Hydra -> JSVal -> JSVal -> JSVal -> JSVal
foreign import javascript safe "$1.synth.solid()" _solid0 :: Hydra -> JSVal
foreign import javascript safe "$1.synth.solid($2)" _solid1 :: Hydra -> JSVal -> JSVal
foreign import javascript safe "$1.synth.solid($2,$3)" _solid2 :: Hydra -> JSVal -> JSVal -> JSVal
foreign import javascript safe "$1.synth.solid($2,$3,$4)" _solid3 :: Hydra -> JSVal -> JSVal -> JSVal -> JSVal
foreign import javascript safe "$1.synth.solid($2,$3,$4,$5)" _solid4 :: Hydra -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal
foreign import javascript safe "$1.synth.gradient()" _gradient0 :: Hydra -> JSVal
foreign import javascript safe "$1.synth.gradient($2)" _gradient1 :: Hydra -> JSVal -> JSVal
foreign import javascript safe "$1.synth.noise()" _noise0 :: Hydra -> JSVal
foreign import javascript safe "$1.synth.noise($2)" _noise1 :: Hydra -> JSVal -> JSVal
foreign import javascript safe "$1.synth.noise($2,$3)" _noise2 :: Hydra -> JSVal -> JSVal -> JSVal
foreign import javascript safe "$1.synth.shape()" _shape0 :: Hydra -> JSVal
foreign import javascript safe "$1.synth.shape($2)" _shape1 :: Hydra -> JSVal -> JSVal
foreign import javascript safe "$1.synth.shape($2,$3)" _shape2 :: Hydra -> JSVal -> JSVal -> JSVal
foreign import javascript safe "$1.synth.shape($2,$3,$4)" _shape3 :: Hydra -> JSVal -> JSVal -> JSVal -> JSVal
foreign import javascript safe "$1.synth.voronoi()" _voronoi0 :: Hydra -> JSVal
foreign import javascript safe "$1.synth.voronoi($2)" _voronoi1 :: Hydra -> JSVal -> JSVal
foreign import javascript safe "$1.synth.voronoi($2,$3)" _voronoi2 :: Hydra -> JSVal -> JSVal -> JSVal
foreign import javascript safe "$1.synth.voronoi($2,$3,$4)" _voronoi3 :: Hydra -> JSVal -> JSVal -> JSVal -> JSVal

outputToJSVal :: Hydra -> Output -> JSVal
outputToJSVal h O0 = _o0 h
outputToJSVal h O1 = _o1 h
outputToJSVal h O2 = _o2 h
outputToJSVal h O3 = _o3 h

foreign import javascript safe "$1.synth.o0" _o0 :: Hydra -> JSVal
foreign import javascript safe "$1.synth.o1" _o1 :: Hydra -> JSVal
foreign import javascript safe "$1.synth.o2" _o2 :: Hydra -> JSVal
foreign import javascript safe "$1.synth.o3" _o3 :: Hydra -> JSVal

evaluateStatement :: Hydra -> Statement -> IO ()
evaluateStatement h (Out s o) = _out (sourceToJSVal h s) (outputToJSVal h o)
evaluateStatement h (Render o) = _render h (outputToJSVal h o)

foreign import javascript safe
  "$1.out($2);"
  _out :: JSVal -> JSVal -> IO ()

foreign import javascript safe
  "$1.synth.render($2);"
  _render :: Hydra -> JSVal -> IO ()
