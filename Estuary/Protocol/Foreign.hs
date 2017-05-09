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

foreign import javascript unsafe
  "$r = new EstuaryProtocol()"
  estuaryProtocolFFI :: IO T.JSVal

foreign import javascript unsafe
  "$1.setUrl($2)"
  setUrlFFI :: T.JSVal -> T.JSVal -> IO ()

foreign import javascript unsafe
  "$1.send($2)"
  sendFFI :: T.JSVal -> T.JSVal -> IO ()

data EstuaryProtocolObject = EstuaryProtocolObject T.JSVal

estuaryProtocol :: IO EstuaryProtocolObject
estuaryProtocol = do
  x <- estuaryProtocolFFI
  return $ EstuaryProtocolObject x

setUrl :: EstuaryProtocolObject -> String -> IO ()
setUrl (EstuaryProtocolObject x) url = setUrlFFI x (Prim.toJSString url)

send :: EstuaryProtocolObject -> String -> IO ()
send (EstuaryProtocolObject x) y = sendFFI x (Prim.toJSString y)

