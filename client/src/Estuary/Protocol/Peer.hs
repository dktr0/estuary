{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Protocol.Peer where

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

foreign import javascript safe
  "new PeerProtocol()"
  newPeerProtocol :: IO T.JSVal

foreign import javascript safe
  "$1.startStreaming();"
  startStreaming :: T.JSVal -> IO ()

foreign import javascript safe
  "$1.id"
  peerProtocolId :: T.JSVal -> IO Text

{- peerProtocolId :: T.JSVal -> IO String
peerProtocolId x = do
  y <- peerProtocolId_ x
  return $ Prim.fromJSString y -}
