{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

module Estuary.Render.SuperDirt (
  SuperDirt,
  newSuperDirt,
  setActive,
  playSample
  ) where

import GHCJS.Types
import GHCJS.Marshal.Pure
import Data.Time
import Data.Time.Clock.POSIX
import qualified Sound.Tidal.Context as Tidal
import Language.Javascript.JSaddle.Object
import Control.Monad (when)

import Estuary.Render.WebDirt hiding (playSample)
import Estuary.Types.NoteEvent

newtype SuperDirt = SuperDirt JSVal

instance PToJSVal SuperDirt where pToJSVal (SuperDirt x) = x

instance PFromJSVal SuperDirt where pFromJSVal = SuperDirt

foreign import javascript unsafe
  "new SuperDirt()"
  newSuperDirt :: IO SuperDirt

foreign import javascript unsafe
  "$1.setActive($2)"
  setActive :: SuperDirt -> Bool -> IO ()

playSample :: SuperDirt -> NoteEvent -> IO ()
playSample sd x = noteEventToSuperDirtJSVal x >>= _playSample sd

foreign import javascript unsafe
  "try { $1.playSample($2) } catch(e) { console.log(e)} "
  _playSample :: SuperDirt -> JSVal -> IO ()

-- transfer whenPosix field to when for SuperDirt
noteEventToSuperDirtJSVal :: NoteEvent -> IO JSVal
noteEventToSuperDirtJSVal (NoteEvent j) = do
  o <- makeObject j
  props <- listProps o
  when (elem "whenPosix" props) $ do
    wPosix <- unsafeGetProp "whenPosix" o
    unsafeSetProp "when" wPosix o
  pure j

