{-# LANGUAGE JavaScriptFFI #-}

module Estuary.WebDirt.SuperDirt (SuperDirt, superDirt, playSample) where
import qualified Sound.Tidal.Context as Tidal
import Data.Map
import Data.Maybe
import GHC.IORef
import qualified GHCJS.Prim as Prim
import qualified GHCJS.Types as T
import qualified GHCJS.Foreign as F
import qualified GHCJS.Marshal.Pure as P
import JavaScript.Object.Internal as O
import GHCJS.Foreign.Internal
import GHCJS.Marshal.Pure
import Estuary.WebDirt.Foreign (createObjFromMap)

data SuperDirt = SuperDirt T.JSVal

foreign import javascript unsafe
  "$r = new SuperDirt()"
  superDirt_ :: IO T.JSVal

foreign import javascript unsafe
  "try { $1.playSample($2) } catch(e) { console.log(e)} "
  playSample_ :: T.JSVal -> T.JSVal -> IO ()

superDirt :: IO SuperDirt
superDirt = superDirt_ >>= return . SuperDirt

playSample :: SuperDirt -> (Double,Tidal.ParamMap) -> IO ()
playSample (SuperDirt x) (t,e) = do
  object <- createObjFromMap t e
  playSample_ x object
