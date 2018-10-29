{-# LANGUAGE JavaScriptFFI #-}

-- Note: this module is currently retained only to provide createObjFromMap
-- It should probably be renamed or combined with something else

module Estuary.WebDirt.Foreign where
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


createObjFromMap:: Double -> Tidal.ParamMap -> IO T.JSVal
createObjFromMap when paramMap = do
  let paramMap' = mapKeys (\x-> case Tidal.name x of "n"->"sample_n"; "s"->"sample_name";otherwise->Tidal.name x) paramMap -- Map String (Value)
--  let paramMap'' = Data.Map.map (maybe (T.nullRef) (valueToJSVal)) paramMap':: Map String T.JSVal -- Map String JSVal
  let paramMap'' = Data.Map.map (valueToJSVal) paramMap':: Map String T.JSVal -- Map String JSVal
  obj <- createEmpty
  --x <- return $ mapWithKey (\k v -> addProp obj (P.pToJSVal k) v) paramMap''
  a<-addProps obj (("when",P.pToJSVal when):(toList paramMap''))
  return a

addProps::T.JSVal -> [(String,T.JSVal)] -> IO (T.JSVal)
addProps obj [] = return obj
addProps obj paramList = do
  obj' <- addProp obj (P.pToJSVal $ fst (paramList!!0)) (snd $ paramList!!0)
  addProps obj' (tail paramList)

valueToJSVal :: Tidal.Value -> T.JSVal
valueToJSVal (Tidal.VI x) = P.pToJSVal x
valueToJSVal (Tidal.VF x) = P.pToJSVal x
valueToJSVal (Tidal.VS x) = P.pToJSVal x


-- FFI below this line:

foreign import javascript unsafe "$r = {};" createEmpty:: IO T.JSVal

foreign import javascript safe
  "$1[$2] = $3;$r=$1"
  addProp :: T.JSVal -> T.JSVal -> T.JSVal -> IO (T.JSVal)
