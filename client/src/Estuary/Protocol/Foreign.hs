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
import Data.Text (Text)

import Estuary.Types.Request
import Estuary.Types.Response

foreign import javascript unsafe
  "__debugEstuaryProtocol = new EstuaryProtocol(); $r = __debugEstuaryProtocol"
  estuaryProtocol_ :: IO T.JSVal

foreign import javascript unsafe
  "$1.setUrl($2)"
  setUrl_ :: T.JSVal -> Text -> IO ()

foreign import javascript unsafe
  "$r = location.hostname"
  getHostName_ :: IO Text

foreign import javascript unsafe
  "$r = location.port"
  getPort_ :: IO Text

foreign import javascript unsafe
  "$1.send($2)"
  send_ :: T.JSVal -> Text -> IO ()

foreign import javascript unsafe
  "$r = $1.getResponses()"
  getResponses_ :: T.JSVal -> IO T.JSVal

foreign import javascript unsafe
  "$r = $1.status"
  getStatus_ :: T.JSVal -> IO Text

data EstuaryProtocolObject = EstuaryProtocolObject T.JSVal

estuaryProtocol :: IO EstuaryProtocolObject
estuaryProtocol = EstuaryProtocolObject <$> estuaryProtocol_

setUrl :: EstuaryProtocolObject -> Text -> IO ()
setUrl (EstuaryProtocolObject x) url = setUrl_ x url

send :: EstuaryProtocolObject -> Text -> IO ()
send (EstuaryProtocolObject x) y = send_ x y

getResponses :: EstuaryProtocolObject -> IO (Either String [Response]) -- left=parsing error right=responses
getResponses (EstuaryProtocolObject x) = (f . decode . Prim.fromJSString) <$> getResponses_ x
  where f (Ok xs) = Right xs
        f (Error x) = Left $ "error trying to parse as [EstuaryProtocol]: " ++ x

getStatus :: EstuaryProtocolObject -> IO Text
getStatus (EstuaryProtocolObject j) = getStatus_ j

getHostName :: IO Text
getHostName = getHostName_

getPort :: IO Text
getPort = getPort_
