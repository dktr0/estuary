{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Protocol.Foreign where

import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import Data.Aeson
import Data.Text (Text)
-- import Foreign.JavaScript.Utils (jsonDecode)

import Estuary.Types.Request
import Estuary.Types.Response

newtype EstuaryProtocolObject = EstuaryProtocolObject JSVal

instance PToJSVal EstuaryProtocolObject where pToJSVal (EstuaryProtocolObject x) = x

instance PFromJSVal EstuaryProtocolObject where pFromJSVal = EstuaryProtocolObject

foreign import javascript unsafe
  "__debugEstuaryProtocol = new EstuaryProtocol(); $r = __debugEstuaryProtocol"
  estuaryProtocol :: IO EstuaryProtocolObject

foreign import javascript unsafe
  "$1.setUrl($2)"
  setUrl :: EstuaryProtocolObject -> Text -> IO ()

foreign import javascript unsafe
  "$r = location.hostname"
  getHostName :: IO Text

foreign import javascript unsafe
  "$r = location.port"
  getPort :: IO Text

foreign import javascript unsafe
  "$1.send($2)"
  send :: EstuaryProtocolObject -> Text -> IO ()

getResponses :: EstuaryProtocolObject -> IO (Either String [Response])
getResponses x = do
  j <- getResponses_ x
  v <- fromJSValUnchecked j -- *** ? maybe dangerous ???
  let p = fromJSON (v :: Value)
  return $ f p
  where
    f (Success a) = Right a
    f (Error e) = Left $ "error trying to parse as [Response]: " ++ e

foreign import javascript unsafe
  "$r = $1.getResponses()"
  getResponses_ :: EstuaryProtocolObject -> IO JSVal

foreign import javascript unsafe
  "$r = $1.status"
  getStatus :: EstuaryProtocolObject -> IO Text
