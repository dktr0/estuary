{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Protocol.Peer where

import GHCJS.Marshal.Pure
import GHCJS.Types
import Data.Text (Text)

newtype PeerProtocol = PeerProtocol JSVal

instance PToJSVal PeerProtocol where pToJSVal (PeerProtocol x) = x

instance PFromJSVal PeerProtocol where pFromJSVal = PeerProtocol

foreign import javascript safe
  "new PeerProtocol()"
  newPeerProtocol :: IO PeerProtocol

foreign import javascript safe
  "$1.startStreaming();"
  startStreaming :: PeerProtocol -> IO ()

foreign import javascript safe
  "$1.id"
  peerProtocolId :: PeerProtocol -> IO Text
