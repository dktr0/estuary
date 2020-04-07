{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.ServerState where

-- This module presents the type ServerState which represents everything that an
-- Estuary server keeps track of in memory as it runs.

import qualified Network.WebSockets as WS
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Control.Concurrent.STM
import Data.Text
import Data.Time

import Estuary.Types.Client
import Estuary.Types.Response
import qualified Estuary.Types.EnsembleS as E

data ServerState = ServerState {
  administrativePassword :: Text,
  nextClientHandle :: TVar Int,
  clients :: TVar (IntMap.IntMap (TVar Client)),
  ensembles :: TVar (Map.Map Text (TVar E.EnsembleS))
}

newServerState :: Text -> Map.Map Text E.EnsembleS -> IO ServerState
newServerState pwd es = atomically $ do
  c <- newTVar IntMap.empty
  es' <- mapM newTVar es
  es'' <- newTVar es'
  nch <- newTVar 0
  return $ ServerState {
    administrativePassword = pwd,
    nextClientHandle = nch,
    clients = c,
    ensembles = es''
  }

addClient :: ServerState -> WS.Connection -> IO ClientHandle
addClient ss x = do
  t <- getCurrentTime
  atomically $ do
    oldMap <- readTVar (clients ss)
    i <- readTVar (nextClientHandle ss)
    c <- newTVar $ newClient t i x
    let newMap = IntMap.insert i c oldMap
    writeTVar (clients ss) newMap
    writeTVar (nextClientHandle ss) (i+1)
    return i

deleteEnsemble :: ServerState -> Text -> IO ()
deleteEnsemble ss eName = atomically $ do
    oldMap <- readTVar (ensembles ss)
    writeTVar (ensembles ss) $ Map.delete eName oldMap
