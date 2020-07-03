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
-- sourceToJSVal _ (List xs) = pToJSVal xs
-- sourceToJSVal h (Fast x) = _fast h (maybeSourceToJSVal h x)
sourceToJSVal h (Osc x y z) = _osc h (maybeSourceToJSVal h x) (maybeSourceToJSVal h y) (maybeSourceToJSVal h z)
sourceToJSVal h (Solid w x y z) = _solid h (maybeSourceToJSVal h w) (maybeSourceToJSVal h x) (maybeSourceToJSVal h y) (maybeSourceToJSVal h z)
sourceToJSVal h (Gradient x) = _gradient h (maybeSourceToJSVal h x)
sourceToJSVal h (Noise x y) = _noise h (maybeSourceToJSVal h x) (maybeSourceToJSVal h y)
sourceToJSVal h (Shape x y z) = _shape h (maybeSourceToJSVal h x) (maybeSourceToJSVal h y) (maybeSourceToJSVal h z)
sourceToJSVal h (Voronoi x y z) = _voronoi h (maybeSourceToJSVal h x) (maybeSourceToJSVal h y) (maybeSourceToJSVal h z)

foreign import javascript safe "$1.fast($2)" _fast :: JSVal -> JSVal -> JSVal
foreign import javascript safe "$1.synth.osc($2,$3,$4)" _osc :: Hydra -> JSVal -> JSVal -> JSVal -> JSVal
foreign import javascript safe "$1.synth.solid($2,$3,$4,$5)" _solid :: Hydra -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal
foreign import javascript safe "$1.synth.gradient($2)" _gradient :: Hydra -> JSVal -> JSVal
foreign import javascript safe "$1.synth.noise($2,$3)" _noise :: Hydra -> JSVal -> JSVal -> JSVal
foreign import javascript safe "$1.synth.shape($2,$3,$4)" _shape :: Hydra -> JSVal -> JSVal -> JSVal -> JSVal
foreign import javascript safe "$1.synth.voronoi($2,$3,$4)" _voronoi :: Hydra -> JSVal -> JSVal -> JSVal -> JSVal

maybeSourceToJSVal :: Hydra -> Maybe Source -> JSVal
maybeSourceToJSVal _ Nothing = nullRef
maybeSourceToJSVal h (Just x) = sourceToJSVal h x

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
