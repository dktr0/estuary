{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Protocol.Foreign where

import GHC.IORef
import qualified GHCJS.Prim as Prim
import qualified GHCJS.Types as T
import qualified GHCJS.Foreign as F
import qualified GHCJS.Marshal.Pure as P
import JavaScript.Object.Internal as O
import GHCJS.Foreign.Internal
import GHCJS.Marshal.Pure
import Text.JSON

import Estuary.Types.Request
import Estuary.Types.Response

foreign import javascript unsafe
  "__debugEstuaryProtocol = new EstuaryProtocol(); $r = __debugEstuaryProtocol"
  estuaryProtocol_ :: IO T.JSVal

foreign import javascript unsafe
  "$1.setUrl($2)"
  setUrl_ :: T.JSVal -> T.JSVal -> IO ()

foreign import javascript unsafe
  "$r = location.hostname"
  getHostName_ :: IO T.JSVal

foreign import javascript unsafe
  "$r = location.port"
  getPort_ :: IO T.JSVal

foreign import javascript unsafe
  "$1.send($2)"
  send_ :: T.JSVal -> T.JSVal -> IO ()

foreign import javascript unsafe
  "$r = $1.getResponses()"
  getResponses_ :: T.JSVal -> IO T.JSVal

foreign import javascript unsafe
  "$r = $1.status"
  getStatus_ :: T.JSVal -> IO T.JSVal

data EstuaryProtocolObject = EstuaryProtocolObject T.JSVal

estuaryProtocol :: IO EstuaryProtocolObject
estuaryProtocol = EstuaryProtocolObject <$> estuaryProtocol_

setUrl :: EstuaryProtocolObject -> String -> IO ()
setUrl (EstuaryProtocolObject x) url = setUrl_ x (Prim.toJSString url)

send :: EstuaryProtocolObject -> String -> IO ()
send (EstuaryProtocolObject x) y = send_ x (Prim.toJSString y)

getResponses :: EstuaryProtocolObject -> IO (Either String [Response]) -- left=parsing error right=responses
getResponses (EstuaryProtocolObject x) = (f . decode . Prim.fromJSString) <$> getResponses_ x
  where f (Ok xs) = Right xs
        f (Error x) = Left $ "error trying to parse as [EstuaryProtocol]: " ++ x

getStatus :: EstuaryProtocolObject -> IO String
getStatus (EstuaryProtocolObject j) = Prim.fromJSString <$> getStatus_ j

getHostName :: IO String
getHostName = Prim.fromJSString <$> getHostName_

getPort :: IO String
getPort = Prim.fromJSString <$> getPort_
