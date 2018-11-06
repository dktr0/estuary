module Estuary.Types.Client where

import qualified Network.WebSockets as WS

type ClientHandle = Int

data Client = Client {
  handle :: ClientHandle,
  connection :: WS.Connection,
  authenticated :: Bool,
  ensemble :: Maybe String,
  authenticatedInEnsemble :: Bool
}

newClient :: ClientHandle -> WS.Connection -> Client
newClient h c = Client {
  handle = h,
  connection = c,
  authenticated = False,
  ensemble = Nothing,
  authenticatedInEnsemble = False
}

setAuthenticatedInEnsemble :: Bool -> Client -> Client
setAuthenticatedInEnsemble x c = c { authenticatedInEnsemble = x }




