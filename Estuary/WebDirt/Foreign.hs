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

foreign import javascript unsafe
  "$r = new WebDirt('WebDirt/sampleMap.json','Dirt/samples',null, function() {console.log('callback from WebDirt constructor completed');});"
  webDirt :: IO T.JSVal

foreign import javascript unsafe
  "try { $1.initializeWebAudio() } catch(e) { console.log(e) }"
  initializeWebAudio :: T.JSVal -> IO ()

foreign import javascript unsafe
  "try { $1.playSample($2)} catch(e) { console.log(e)} "
  playSample':: T.JSVal -> T.JSVal -> IO ()

foreign import javascript unsafe
  "try { $r = $1.getCurrentTime() } catch(e) { console.log(e)} "
  getCurrentTime :: T.JSVal -> IO Double

playSample::T.JSVal -> (Double,ParamMap) -> IO()
playSample webDirt (t,e) = do
  object <- createObjFromMap t e
  playSample' webDirt object
  return ()

foreign import javascript unsafe "$r = {};" createEmpty:: IO T.JSVal

createObjFromMap:: Double -> ParamMap -> IO T.JSVal
createObjFromMap when paramMap = do
  let paramMap' = mapKeys (\x-> case name x of "n"->"sample_n"; "s"->"sample_name";otherwise->name x) paramMap -- Map String (Value)
  let paramMap'' = Data.Map.map (maybe (T.nullRef) (valueToJSVal)) paramMap':: Map String T.JSVal -- Map String JSVal
  obj <- createEmpty
  --x <- return $ mapWithKey (\k v -> addProp obj (P.pToJSVal k) v) paramMap''
  a<-addProps obj (("when",P.pToJSVal when):(toList paramMap''))
  return a

foreign import javascript safe
  "$1[$2] = $3;$r=$1"
  addProp :: T.JSVal -> T.JSVal -> T.JSVal -> IO (T.JSVal)

addProps::T.JSVal -> [(String,T.JSVal)] -> IO (T.JSVal)
addProps obj [] = return obj
addProps obj paramList = do
  obj' <- addProp obj (P.pToJSVal $ fst (paramList!!0)) (snd $ paramList!!0)
  addProps obj' (tail paramList)

valueToJSVal :: Value -> T.JSVal
valueToJSVal (VI x) = P.pToJSVal x
valueToJSVal (VF x) = P.pToJSVal x
valueToJSVal (VS x) = P.pToJSVal x

foreign import javascript unsafe
  "$1.syncWithEsp($2)"
  syncWithEsp' :: T.JSVal -> T.JSVal -> IO ()

syncWithEsp :: T.JSVal -> String -> IO ()
syncWithEsp webDirt url = syncWithEsp' webDirt (pToJSVal url)

foreign import javascript unsafe
  "$1.setTempo({time:$2,beats:$3,bpm:$4})"
  setTempo :: T.JSVal -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe
  "$r = $1.tempo"
  tempo' :: T.JSVal -> IO T.JSVal

foreign import javascript safe
  "$r = $1[$2]"
  deindexJSObject :: T.JSVal -> T.JSVal -> IO T.JSVal

tempo :: T.JSVal -> IO (Double,Double,Double)
tempo w = do
  x <- tempo' w
  time <- deindexJSObject x (pToJSVal "time")
  beats <- deindexJSObject x (pToJSVal "beats")
  bpm <- deindexJSObject x (pToJSVal "bpm")
  return (pFromJSVal time,pFromJSVal beats,pFromJSVal bpm)
