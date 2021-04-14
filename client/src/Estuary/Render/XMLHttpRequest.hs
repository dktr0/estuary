module Estuary.Render.XMLHttpRequest where

import Data.Text
import GHCJS.Types
import GHCJS.Marshal.Pure
import GHCJS.Foreign.Callback


newtype XMLHttpRequest = XMLHttpRequest JSVal

instance PFromJSVal XMLHttpRequest where pFromJSVal x = XMLHttpRequest x

instance PToJSVal XMLHttpRequest where pToJSVal (XMLHttpRequest x) = x

foreign import javascript safe
  "$r = new XMLHttpRequest(); $r.open('GET',$1,true); $r.responseType='text';"
  textXMLHttpRequest :: Text -> IO XMLHttpRequest

foreign import javascript safe
  "$r = new XMLHttpRequest(); $r.open('GET',$1,true); $r.responseType='json';"
  jsonXMLHttpRequest :: Text -> IO XMLHttpRequest

foreign import javascript safe
  "$r = new XMLHttpRequest(); $r.open('GET',$1,true); $r.responseType='arraybuffer';"
  arraybufferXMLHttpRequest :: Text -> IO XMLHttpRequest

foreign import javascript safe
  "$1.onload = $2($1.response);"
  onLoad :: XMLHttpRequest -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript safe
  "$1.onerror = $2;"
  onError :: XMLHttpRequest -> Callback (IO ()) -> IO ()

foreign import javascript safe
  "$1.onabort = $2;"
  onAbort :: XMLHttpRequest -> Callback (IO ()) -> IO ()

foreign import javascript safe
  "$1.send();"
  send :: XMLHttpRequest -> IO ()
