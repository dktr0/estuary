{-# LANGUAGE JavaScriptFFI #-}

module Estuary.WebDirt.SuperDirt (SuperDirt, newSuperDirt, setActive) where

import qualified GHCJS.Types as T
import qualified Sound.Tidal.Context as Tidal
import qualified Estuary.WebDirt.SampleEngine as S
import Estuary.WebDirt.Foreign (createObjFromMap)


newtype SuperDirt = SuperDirt T.JSVal

instance S.SampleEngine SuperDirt where
  getClockDiff _ = return 0.0 -- *** TODO: broken since change in interpretation of getClockDiff
  playSample sd x = playSample sd x
  getPeakLevels sd = return []
  getRmsLevels sd = return []

newSuperDirt :: IO SuperDirt
newSuperDirt = superDirt_ >>= return . SuperDirt

setActive :: SuperDirt -> Bool -> IO ()
setActive (SuperDirt j) x = setActive_ j x

playSample :: SuperDirt -> (Double,Tidal.ControlMap) -> IO ()
playSample (SuperDirt x) (t,e) = do
  object <- createObjFromMap t e
  playSample_ x object

-- FFI below this line:

foreign import javascript unsafe
  "new SuperDirt()"
  superDirt_ :: IO T.JSVal

foreign import javascript unsafe
  "$1.setActive($2)"
  setActive_ :: T.JSVal -> Bool -> IO ()

foreign import javascript unsafe
  "try { $1.playSample($2) } catch(e) { console.log(e)} "
  playSample_ :: T.JSVal -> T.JSVal -> IO ()
