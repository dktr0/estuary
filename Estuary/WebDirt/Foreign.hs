{-# LANGUAGE JavaScriptFFI #-}

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
import Control.Monad.IO.Class (liftIO)
import Reflex
import Reflex.Dom

import Estuary.Types.Hint

data WebDirt = WebDirt T.JSVal

--foreign import javascript unsafe
--  "$r = new WebDirt('WebDirt/sampleMap.json','Dirt/samples',null, function() {console.log('callback from WebDirt constructor completed');});"
--  webDirt :: IO T.JSVal

foreign import javascript unsafe
  "$r = ___globalWebDirt"
  webDirt_ :: IO T.JSVal

webDirt :: IO WebDirt
webDirt = webDirt_ >>= return . WebDirt

--foreign import javascript unsafe
--  "try { $1.initializeWebAudio() } catch(e) { console.log(e) }"
--  initializeWebAudio :: T.JSVal -> IO ()

-- foreign import javascript unsafe
--   "console.log($1)"
--  testLog:: T.JSVal -> IO ()

foreign import javascript unsafe
  "try { $r = $1.getCurrentTime() } catch(e) { console.log(e)} "
  getCurrentTime_ :: T.JSVal -> IO Double

getCurrentTime :: WebDirt -> IO Double
getCurrentTime (WebDirt j) = getCurrentTime_ j

foreign import javascript unsafe
  "try { ___globalWebDirt.playSample($2)} catch(e) { console.log(e)} "
  playSample_ :: T.JSVal -> T.JSVal -> IO ()

playSample :: WebDirt -> (Double,Tidal.ParamMap) -> IO ()
playSample (WebDirt j) (t,e) = do
  object <- createObjFromMap t e
  playSample_ j object

foreign import javascript unsafe "$r = {};" createEmpty:: IO T.JSVal

createObjFromMap:: Double -> Tidal.ParamMap -> IO T.JSVal
createObjFromMap when paramMap = do
  let paramMap' = mapKeys (\x-> case Tidal.name x of "n"->"sample_n"; "s"->"sample_name";otherwise->Tidal.name x) paramMap -- Map String (Value)
--  let paramMap'' = Data.Map.map (maybe (T.nullRef) (valueToJSVal)) paramMap':: Map String T.JSVal -- Map String JSVal
  let paramMap'' = Data.Map.map (valueToJSVal) paramMap':: Map String T.JSVal -- Map String JSVal
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

valueToJSVal :: Tidal.Value -> T.JSVal
valueToJSVal (Tidal.VI x) = P.pToJSVal x
valueToJSVal (Tidal.VF x) = P.pToJSVal x
valueToJSVal (Tidal.VS x) = P.pToJSVal x

foreign import javascript unsafe
  "$1.syncWithEsp($2)"
  syncWithEsp_ :: T.JSVal -> T.JSVal -> IO ()

syncWithEsp :: WebDirt -> String -> IO ()
syncWithEsp (WebDirt j) url = syncWithEsp_ j (pToJSVal url)

foreign import javascript unsafe
  "$1.setTempo({time:$2,beats:$3,bpm:$4})"
  setTempo_ :: T.JSVal -> Double -> Double -> Double -> IO ()

setTempo :: WebDirt -> Double -> Double -> Double -> IO ()
setTempo (WebDirt j) time beats bpm = setTempo_ j time beats bpm

foreign import javascript unsafe
  "$r = $1.tempo"
  tempo_ :: T.JSVal -> IO T.JSVal

foreign import javascript safe
  "$r = $1[$2]"
  deindexJSObject :: T.JSVal -> T.JSVal -> IO T.JSVal

tempo :: WebDirt -> IO (Double,Double,Double)
tempo (WebDirt j) = do
  x <- tempo_ j
  time <- deindexJSObject x (pToJSVal "time")
  beats <- deindexJSObject x (pToJSVal "beats")
  bpm <- deindexJSObject x (pToJSVal "bpm")
  return (pFromJSVal time,pFromJSVal beats,pFromJSVal bpm)


foreign import javascript safe
  "$1.sampleHint($2)"
  sampleHint_ :: T.JSVal -> T.JSVal -> IO ()

doHint :: WebDirt -> Hint -> IO ()
doHint (WebDirt j) (SampleHint x) = sampleHint_ j (P.pToJSVal x)

performHint :: MonadWidget t m => WebDirt -> Event t Hint -> m ()
performHint wd ev = performEvent_ $ fmap (liftIO . (doHint wd)) ev
