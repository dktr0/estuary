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
import Estuary.Protocol.JSON
import Text.JSON

foreign import javascript unsafe
  "__debugEstuaryProtocol = new EstuaryProtocol(); $r = __debugEstuaryProtocol"
  estuaryProtocol_ :: IO T.JSVal

foreign import javascript unsafe
  "$1.setUrl($2)"
  setUrl_ :: T.JSVal -> T.JSVal -> IO ()

foreign import javascript unsafe
  "$1.send($2)"
  send_ :: T.JSVal -> T.JSVal -> IO ()

foreign import javascript unsafe
  "$r = $1.getEdits()"
  getEdits_ :: T.JSVal -> IO T.JSVal

foreign import javascript unsafe
  "$r = $1.status"
  getStatus_ :: T.JSVal -> IO T.JSVal

data EstuaryProtocolObject = EstuaryProtocolObject T.JSVal

estuaryProtocol :: IO EstuaryProtocolObject
estuaryProtocol = estuaryProtocol_ >>= return . EstuaryProtocolObject

setUrl :: EstuaryProtocolObject -> String -> IO ()
setUrl (EstuaryProtocolObject x) url = setUrl_ x (Prim.toJSString url)

send :: EstuaryProtocolObject -> String -> IO ()
send (EstuaryProtocolObject x) y = send_ x (Prim.toJSString y)

getEdits :: EstuaryProtocolObject -> IO [EstuaryProtocol]
getEdits (EstuaryProtocolObject x) = getEdits_ x >>= return . f . decode . Prim.fromJSString
  where f (Ok xs) = xs
        f (Error x) = [ProtocolError ("error trying to parse as [EstuaryProtocol]: " ++ x)]

getStatus :: EstuaryProtocolObject -> IO String
getStatus (EstuaryProtocolObject j) = getStatus_ j >>= return . Prim.fromJSString
