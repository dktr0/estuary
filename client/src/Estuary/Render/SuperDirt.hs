{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Render.SuperDirt (
  SuperDirt,
  newSuperDirt,
  setActive,
  playSample,
  noteEventToSuperDirtJSVal
  ) where

import GHCJS.Types
import GHCJS.Marshal.Pure
import Data.Time
import Data.Time.Clock.POSIX
import qualified Sound.Tidal.Context as Tidal

-- import Estuary.Types.Tempo
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

foreign import javascript unsafe
  "try { $1.playSample($2) } catch(e) { console.log(e)} "
  playSample :: SuperDirt -> JSVal -> IO ()

noteEventToSuperDirtJSVal :: NoteEvent -> IO JSVal
noteEventToSuperDirtJSVal (utc,m) = do
  let t = realToFrac $ utcTimeToPOSIXSeconds utc
  mapTextJSValToJSVal (t,fmap datumToJSVal m)
