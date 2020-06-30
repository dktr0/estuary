{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Estuary.Types.ServerState where

-- This module presents the type ServerState which represents everything that an
-- Estuary server keeps track of in memory as it runs.

import Network.Socket (SockAddr)
import qualified Network.WebSockets as WS
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Control.Concurrent.STM hiding (atomically,readTVarIO)
import Data.Text
import Data.Time

import Estuary.Types.Client
import Estuary.Types.Response
import qualified Estuary.Types.EnsembleS as E
import Estuary.AtomicallyTimed

data ServerState = ServerState {
  moderatorPassword :: Text, -- to make announcements, delete/change ensembles, block IP addresses, etc
  communityPassword :: Text, -- to make ensembles with longer/permanent expiry times
  nextClientHandle :: TVar Int,
  clients :: TVar (IntMap.IntMap (TVar Client)),
  clientCount :: TVar Int,
  ensembles :: TVar (Map.Map Text (TVar E.EnsembleS))
}

newServerState :: Text -> Text -> Map.Map Text E.EnsembleS -> IO ServerState
newServerState mpwd cpwd es = $atomically $ do
  c <- newTVar IntMap.empty
  es' <- mapM newTVar es
  es'' <- newTVar es'
  nch <- newTVar 0
  cc <- newTVar 0
  return $ ServerState {
    moderatorPassword = mpwd,
    communityPassword = cpwd,
    nextClientHandle = nch,
    clients = c,
    clientCount = cc,
    ensembles = es''
  }

addClient :: SockAddr -> ServerState -> WS.Connection -> IO (ClientHandle,TVar Client,TChan (ClientHandle,Response))
addClient ip ss x = do
  t <- getCurrentTime
  $atomically $ do
    oldMap <- readTVar (clients ss)
    i <- readTVar (nextClientHandle ss)
    sChan <- newTChan
    c <- newTVar $ newClient ip sChan t i x
    let newMap = IntMap.insert i c oldMap
    writeTVar (clients ss) newMap
    writeTVar (clientCount ss) (IntMap.size newMap)
    writeTVar (nextClientHandle ss) (i+1)
    return (i,c,sChan)

deleteEnsemble :: ServerState -> Text -> IO ()
deleteEnsemble ss eName = $atomically $ do
    oldMap <- readTVar (ensembles ss)
    writeTVar (ensembles ss) $ Map.delete eName oldMap
