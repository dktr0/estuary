{-# LANGUAGE JavaScriptFFI #-}

module Estuary.WebDirt.Foreign where
import Sound.Tidal.Context
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

foreign import javascript safe   "$1[$2] = $3;$r=$1" addProp :: T.JSVal -> T.JSVal -> T.JSVal -> IO (T.JSVal)

foreign import javascript unsafe
  "$r = new WebDirt('WebDirt/sampleMap.json','Dirt/samples',null, function() {console.log('callback from WebDirt constructor completed');});"
  newWebDirt :: IO (T.JSVal)

foreign import javascript unsafe "$r = {};" createEmpty:: IO T.JSVal

foreign import javascript unsafe "console.log($1);" consoleLog::T.JSVal -> IO()

foreign import javascript unsafe "try { $1.queue({sample_name: 'cp', sample_n:0}) } catch(e) {console.log(e)} " webDirtTestMessage ::T.JSVal->IO (T.JSVal)

foreign import javascript unsafe
  "try { $1.queue({whenPosix: $2, sample_name: $3, sample_n: $4})} catch(e) { console.log(e)} "
  playSample':: T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> IO()

foreign import javascript unsafe
  "try { $1.queue($2)} catch(e) { console.log(e)} "
  queue:: T.JSVal -> T.JSVal -> IO()

playSample::T.JSVal -> T.JSVal -> ParamMap -> IO()
playSample webDirt time paramMap= do
  object <- createObjFromMap paramMap--[elems] params
  consoleLog object
  queue webDirt object
  return ()

createObjFromMap:: ParamMap -> IO T.JSVal
createObjFromMap paramMap = do
  let paramMap' = mapKeys (\x-> case name x of "n"->"sample_n"; "s"->"sample_name";otherwise->name x) paramMap -- Map String (Value)
  let paramMap'' = Data.Map.map (maybe (T.nullRef) (valueToJSVal)) paramMap':: Map String T.JSVal -- Map String JSVal
  obj <- createEmpty
  --x <- return $ mapWithKey (\k v -> addProp obj (P.pToJSVal k) v) paramMap''
  a<-addProps obj (toList paramMap'')
  return a

addProps::T.JSVal -> [(String,T.JSVal)] -> IO (T.JSVal)
addProps obj [] = return obj
addProps obj paramList = do
  obj' <- addProp obj (P.pToJSVal $ fst (paramList!!0)) (snd $ paramList!!0)
  addProps obj' (tail paramList)


valueToJSVal :: Value -> T.JSVal
valueToJSVal (VI x) = P.pToJSVal x
valueToJSVal (VF x) = P.pToJSVal x
valueToJSVal (VS x) = P.pToJSVal x
