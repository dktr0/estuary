{-# LANGUAGE JavaScriptFFI #-}

module Estuary.WebDirt.Foreign where
import GHC.IORef
import qualified GHCJS.Prim as Prim
import qualified GHCJS.Types as T
import qualified GHCJS.Foreign as F
import GHCJS.Foreign.Internal
import GHCJS.Marshal.Pure

foreign import javascript unsafe
  "$r = new WebDirt('WebDirt/sampleMap.json','Dirt/samples',null, function() {console.log('callback from WebDirt constructor completed');});"
  newWebDirt :: IO (T.JSVal)

foreign import javascript unsafe "try { $1.queue({sample_name: 'cp', sample_n:0}) } catch(e) {console.log(e)} " webDirtTestMessage ::T.JSVal->IO (T.JSVal)

foreign import javascript unsafe
  "try { $1.queue({whenPosix: $2, sample_name: $3, sample_n: $4})} catch(e) { console.log(e)} "
  playSample':: T.JSVal -> T.JSVal -> T.JSVal -> T.JSVal -> IO()

playSample :: T.JSVal -> Double -> String -> Int -> IO ()
playSample webDirt when sampleName sampleN = playSample' webDirt (pToJSVal when) (Prim.toJSString sampleName) (pToJSVal sampleN)
