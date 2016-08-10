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
--import Data.JSString.Internal.Type
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



-- foreign import javascript unsafe
--   "try { $1.queue({whenPosix: $2, accelerate: $3, bandf:$4, bandq: $5, begin:$6, coarse:$7, crush:$8, cutoff:$9, delay:$10, delayfeedback:$11, delaytime:$12, end:$12, gain:$13, hcutoff:$14, hresonance:$15, loop:$16, sample_n:$17, pan:$18, resonance:$19, shape:$20, speed: $21, sample_name:$22, unit:$23, vowel:$24]})} catch(e) { console.log(e)} "
--   queue:: T.JSVal -> T.JSVal -> [T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal ->T.JSVal->T.JSVal]-> IO()

--playSample'' :: T.JSVal -> Double -> String -> Int -> IO ()
-- playSample'' webDirt when sampleName sampleN = playSample' webDirt (pToJSVal when) (Prim.toJSString sampleName) (pToJSVal sampleN)


-- data Param = S {name :: String, sDefault :: Maybe String} | F {name :: String, fDefault :: Maybe Double}
--            | I {name :: String, iDefault :: Maybe Int}
-- data Value = VS { svalue :: String } | VF { fvalue :: Double } | VI { ivalue :: Int }
-- ParamMap = Map Param (Maybe Value)

playSample::T.JSVal -> T.JSVal -> ParamMap -> IO()
playSample webDirt time paramMap= do
  object <- createObjFromMap paramMap--[elems] params
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
--  snd . last $ toList x

-- createObjFromList:: [T.JSVal] -> IO T.JSVal
-- createObjFromList vals = do
--   let props = Prelude.map (P.pToJSVal) ["accelerate", "bandf", "bandq", "begin", "coarse", "crush", "cutoff", "delay", "delayfeedback", "delaytime", "end", "gain", "hcutoff", "hresonance", "loop", "sample_n", "pan", "resonance", "shape", "speed", "sample_name", "unit", "vowel"]
--   obj <- createEmpty
--   return $ zipWith (\a b -> addProp obj a b) props vals
--   return obj

test = do
  putStrLn "Test:"
  a <- createEmpty
  putStrLn "hmmmmm"
  b <- addProp (P.pToJSVal "sample_name") (P.pToJSVal "bd") a
  consoleLog a
  return ()

valueToJSVal :: Value -> T.JSVal
valueToJSVal (VI x) = P.pToJSVal x
valueToJSVal (VF x) = P.pToJSVal x
valueToJSVal (VS x) = P.pToJSVal x
