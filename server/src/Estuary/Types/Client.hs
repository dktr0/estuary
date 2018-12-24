module Estuary.Types.Client where

import Network.WebSockets

type ClientHandle = Int

data Client = Client {
  handle :: ClientHandle,
  connection :: Connection,
  authenticated :: Bool,
  ensemble :: Maybe String,
  authenticatedInEnsemble :: Bool
}

newClient :: ClientHandle -> Connection -> Client
newClient h c = Client {
  handle = h,
  connection = c,
  authenticated = False,
  ensemble = Nothing,
  authenticatedInEnsemble = False
}

