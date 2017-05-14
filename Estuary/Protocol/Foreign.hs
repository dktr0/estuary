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
  estuaryProtocolFFI :: IO T.JSVal

foreign import javascript unsafe
  "$1.setUrl($2)"
  setUrlFFI :: T.JSVal -> T.JSVal -> IO ()

foreign import javascript unsafe
  "$1.send($2)"
  sendFFI :: T.JSVal -> T.JSVal -> IO ()

foreign import javascript unsafe
  "$r = $1.getTextEdit($2)"
  getTextEditFFI :: T.JSVal -> Int -> IO T.JSVal

foreign import javascript unsafe
  "$r = $1.getEstuaryEdit($2)"
  getEstuaryEditFFI :: T.JSVal -> Int -> IO T.JSVal

foreign import javascript unsafe
  "$r = $1.getEdits()"
  getEditsFFI :: T.JSVal -> IO T.JSVal

data EstuaryProtocolObject = EstuaryProtocolObject T.JSVal

estuaryProtocol :: IO EstuaryProtocolObject
estuaryProtocol = do
  x <- estuaryProtocolFFI
  return $ EstuaryProtocolObject x

setUrl :: EstuaryProtocolObject -> String -> IO ()
setUrl (EstuaryProtocolObject x) url = setUrlFFI x (Prim.toJSString url)

send :: EstuaryProtocolObject -> String -> IO ()
send (EstuaryProtocolObject x) y = sendFFI x (Prim.toJSString y)

{-
getTextEdit :: EstuaryProtocolObject -> Int -> IO EstuaryProtocol
getTextEdit (EstuaryProtocolObject x) n = do 
  y <- getTextEditFFI x n
  return $ TextEdit "" n (Prim.fromJSString y)

getEstuaryEdit :: EstuaryProtocolObject -> Int -> IO EstuaryProtocol
getEstuaryEdit (EstuaryProtocolObject x) n = do 
  y <- getEstuaryEditFFI x n
  return $ f (decode (Prim.fromJSString y))
  where f (Ok (EstuaryEdit p n' z)) = EstuaryEdit p n' z
        f _ = ProtocolError "not EstuaryEdit or some other problem?"
-}

getEdits :: EstuaryProtocolObject -> IO [EstuaryProtocol]
getEdits (EstuaryProtocolObject x) = do 
  y <- getEditsFFI x
  return $ f (decode (Prim.fromJSString y))
  where f (Ok xs) = xs
        f (Error x) = [ProtocolError ("error trying to parse as [EstuaryProtocol]: " ++ x)]

