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


newtype JSParameters = JSParameters JSVal
instance PToJSVal JSParameters where pToJSVal (JSParameters x) = x
instance PFromJSVal JSParameters where pFromJSVal = JSParameters

parametersToJS :: Parameters -> JSParameters
parametersToJS (Parameters []) = _list0
parametersToJS (Parameters (x:[])) = _list1 (pToJSVal x)
parametersToJS (Parameters (x:y:[])) = _list2 (pToJSVal x) (pToJSVal y)
parametersToJS (Parameters (x:y:z:_)) = _list3 (pToJSVal x) (pToJSVal y) (pToJSVal z)

foreign import javascript safe "[]" _list0 :: JSParameters
foreign import javascript safe "[$1]" _list1 :: JSVal -> JSParameters
foreign import javascript safe "[$1,$2]" _list2 :: JSVal -> JSVal -> JSParameters
foreign import javascript safe "[$1,$2,$3]" _list3 :: JSVal -> JSVal -> JSVal -> JSParameters


newtype JSSource = JSSource JSVal
instance PToJSVal JSSource where pToJSVal (JSSource x) = x
instance PFromJSVal JSSource where pFromJSVal = JSSource

sourceToJS :: Hydra -> Source -> JSSource
sourceToJS h (Osc []) = _osc0 h
sourceToJS h (Osc (x:[])) = _osc1 h (parametersToJS x)
sourceToJS h (Osc (x:y:[])) = _osc2 h (parametersToJS x) (parametersToJS y)
sourceToJS h (Osc (x:y:z:_)) = _osc3 h (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (Solid []) = _solid0 h
sourceToJS h (Solid (x:[])) = _solid1 h (parametersToJS x)
sourceToJS h (Solid (x:y:[])) = _solid2 h (parametersToJS x) (parametersToJS y)
sourceToJS h (Solid (x:y:z:[])) = _solid3 h (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (Solid (w:x:y:z:_)) = _solid4 h (parametersToJS w) (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (Gradient []) = _gradient0 h
sourceToJS h (Gradient (x:_)) = _gradient1 h (parametersToJS x)
sourceToJS h (Noise []) = _noise0 h
sourceToJS h (Noise (x:[])) = _noise1 h (parametersToJS x)
sourceToJS h (Noise (x:y:_)) = _noise2 h (parametersToJS x) (parametersToJS y)
sourceToJS h (Shape []) = _shape0 h
sourceToJS h (Shape (x:[])) = _shape1 h (parametersToJS x)
sourceToJS h (Shape (x:y:[])) = _shape2 h (parametersToJS x) (parametersToJS y)
sourceToJS h (Shape (x:y:z:_)) = _shape3 h (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (Voronoi []) = _voronoi0 h
sourceToJS h (Voronoi (x:[])) = _voronoi1 h (parametersToJS x)
sourceToJS h (Voronoi (x:y:[])) = _voronoi2 h (parametersToJS x) (parametersToJS y)
sourceToJS h (Voronoi (x:y:z:_)) = _voronoi3 h (parametersToJS x) (parametersToJS y) (parametersToJS z)
sourceToJS h (Brightness [] s) = _brightness0 (sourceToJS h s)
sourceToJS h (Brightness (x:_) s) = _brightness1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Contrast [] s) = _contrast0 (sourceToJS h s)
sourceToJS h (Contrast (x:_) s) = _contrast1 (sourceToJS h s) (parametersToJS x)
sourceToJS h (Colorama [] s) = _colorama0 (sourceToJS h s)
sourceToJS h (Colorama (x:_) s) = _colorama1 (sourceToJS h s) (parametersToJS x)


foreign import javascript safe "$1.synth.osc()" _osc0 :: Hydra -> JSSource
foreign import javascript safe "$1.synth.osc($2)" _osc1 :: Hydra -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.osc($2,$3)" _osc2 :: Hydra -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.osc($2,$3,$4)" _osc3 :: Hydra -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.solid()" _solid0 :: Hydra -> JSSource
foreign import javascript safe "$1.synth.solid($2)" _solid1 :: Hydra -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.solid($2,$3)" _solid2 :: Hydra -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.solid($2,$3,$4)" _solid3 :: Hydra -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.solid($2,$3,$4,$5)" _solid4 :: Hydra -> JSParameters -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.gradient()" _gradient0 :: Hydra -> JSSource
foreign import javascript safe "$1.synth.gradient($2)" _gradient1 :: Hydra -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.noise()" _noise0 :: Hydra -> JSSource
foreign import javascript safe "$1.synth.noise($2)" _noise1 :: Hydra -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.noise($2,$3)" _noise2 :: Hydra -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.shape()" _shape0 :: Hydra -> JSSource
foreign import javascript safe "$1.synth.shape($2)" _shape1 :: Hydra -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.shape($2,$3)" _shape2 :: Hydra -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.shape($2,$3,$4)" _shape3 :: Hydra -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.voronoi()" _voronoi0 :: Hydra -> JSSource
foreign import javascript safe "$1.synth.voronoi($2)" _voronoi1 :: Hydra -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.voronoi($2,$3)" _voronoi2 :: Hydra -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.synth.voronoi($2,$3,$4)" _voronoi3 :: Hydra -> JSParameters -> JSParameters -> JSParameters -> JSSource
foreign import javascript safe "$1.brightness()" _brightness0 :: JSSource -> JSSource
foreign import javascript safe "$1.brightness($2)" _brightness1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.contrast()" _contrast0 :: JSSource -> JSSource
foreign import javascript safe "$1.contrast($2)" _contrast1 :: JSSource -> JSParameters -> JSSource
foreign import javascript safe "$1.colorama()" _colorama0 :: JSSource -> JSSource
foreign import javascript safe "$1.colorama($2)" _colorama1 :: JSSource -> JSParameters -> JSSource


newtype JSOutput = JSOutput JSVal
instance PToJSVal JSOutput where pToJSVal (JSOutput x) = x
instance PFromJSVal JSOutput where pFromJSVal = JSOutput

outputToJS :: Hydra -> Output -> JSOutput
outputToJS h O0 = _o0 h
outputToJS h O1 = _o1 h
outputToJS h O2 = _o2 h
outputToJS h O3 = _o3 h

foreign import javascript safe "$1.synth.o0" _o0 :: Hydra -> JSOutput
foreign import javascript safe "$1.synth.o1" _o1 :: Hydra -> JSOutput
foreign import javascript safe "$1.synth.o2" _o2 :: Hydra -> JSOutput
foreign import javascript safe "$1.synth.o3" _o3 :: Hydra -> JSOutput


evaluateStatement :: Hydra -> Statement -> IO ()
evaluateStatement h (Out s o) = _out (sourceToJS h s) (outputToJS h o)
evaluateStatement h (Render o) = _render h (outputToJS h o)

foreign import javascript safe
  "$1.out($2);"
  _out :: JSSource -> JSOutput -> IO ()

foreign import javascript safe
  "$1.synth.render($2);"
  _render :: Hydra -> JSOutput -> IO ()
