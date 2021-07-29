module Estuary.Resources.XMLHttpRequest where

-- An XMLHttpRequest is a simple typed wrapper for a JSVal that is an XMLHttpRequest.
-- Functions are provided for creating three kinds of XMLHttpRequest, providing
-- callbacks for the onLoad, onError, and onAbort events, and sending the request.


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
  "$1.onload = function() { $2($1.response); }"
  _onLoad :: XMLHttpRequest -> Callback (JSVal -> IO ()) -> IO ()

onLoad :: XMLHttpRequest -> (JSVal -> IO ()) -> IO ()
onLoad xhr cb = do
  cb' <- asyncCallback1 cb
  _onLoad xhr cb'

foreign import javascript safe
  "$1.onerror = $2;"
  _onError :: XMLHttpRequest -> Callback (IO ()) -> IO ()

onError :: XMLHttpRequest -> IO () -> IO ()
onError xhr cb = do
  cb' <- asyncCallback cb
  _onError xhr cb'

foreign import javascript safe
  "$1.onabort = $2;"
  _onAbort :: XMLHttpRequest -> Callback (IO ()) -> IO ()

onAbort :: XMLHttpRequest -> IO () -> IO ()
onAbort xhr cb = do
  cb' <- asyncCallback cb
  _onAbort xhr cb'

foreign import javascript safe
  "$1.send();"
  send :: XMLHttpRequest -> IO ()
