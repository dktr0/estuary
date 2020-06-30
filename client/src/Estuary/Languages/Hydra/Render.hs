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
  "new Hydra({ canvas:$1, autoLoop: false})"
  newHydra :: HTMLCanvasElement -> IO Hydra

foreign import javascript safe
  "$1.setResolution($2,$3);"
  setResolution :: Hydra -> Int -> Int -> IO ()

evaluate :: [Statement] -> IO ()
evaluate = mapM_ evaluateStatement

foreign import javascript safe
  "$1.tick($2);"
  tick :: Hydra -> Double -> IO () -- second argument is time in millis since last tick

instance PToJSVal Source where
  pToJSVal (ConstantInt x) = pToJSVal x
  pToJSVal (ConstantDouble x) = pToJSVal x
  -- pToJSVal (List xs) = pToJSVal xs
  pToJSVal (Fast x) = _fast (pToJSVal x)
  pToJSVal (Osc x y z) = _osc (pToJSVal x) (pToJSVal y) (pToJSVal z)
  pToJSVal _ = nullRef -- placeholder: should be removed once all patterns are matched

foreign import javascript safe "fast($1)" _fast :: JSVal -> JSVal
foreign import javascript safe "osc($1,$2,$3)" _osc :: JSVal -> JSVal -> JSVal -> JSVal

instance PToJSVal Output where
  pToJSVal O0 = _o0
  pToJSVal O1 = _o1
  pToJSVal O2 = _o2
  pToJSVal O3 = _o3

foreign import javascript safe "o0" _o0 :: JSVal
foreign import javascript safe "o1" _o1 :: JSVal
foreign import javascript safe "o2" _o2 :: JSVal
foreign import javascript safe "o3" _o3 :: JSVal

evaluateStatement :: Statement -> IO ()
evaluateStatement (Out s o) = _out (pToJSVal s) (pToJSVal o)
evaluateStatement (Render o) = _render (pToJSVal o)

foreign import javascript safe
  "$1.out($2);"
  _out :: JSVal -> JSVal -> IO ()

foreign import javascript safe
  "render($1);"
  _render :: JSVal -> IO ()
