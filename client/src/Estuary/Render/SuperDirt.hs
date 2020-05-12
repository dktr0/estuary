{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Render.SuperDirt (SuperDirt, newSuperDirt, setActive, playSample) where

import GHCJS.Types
import GHCJS.Marshal.Pure

newtype SuperDirt = SuperDirt JSVal

instance PToJSVal SuperDirt where pToJSVal (SuperDirt x) = x

instance PFromJSVal SuperDirt where pFromJSVal = SuperDirt

foreign import javascript unsafe
  "new SuperDirt()"
  newSuperDirt :: IO SuperDirt

foreign import javascript unsafe
  "$1.setActive($2)"
  setActive :: SuperDirt -> Bool -> IO ()

foreign import javascript unsafe
  "try { $1.playSample($2) } catch(e) { console.log(e)} "
  playSample :: SuperDirt -> JSVal -> IO ()
