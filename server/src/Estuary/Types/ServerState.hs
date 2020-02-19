{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.ServerState where

-- This module presents the type ServerState which represents everything that an
-- Estuary server keeps track of in memory as it runs.

import qualified Network.WebSockets as WS
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import Control.Concurrent.STM
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Text

import Estuary.Types.Tempo
import Estuary.Types.Client
import Estuary.Types.Definition
import Estuary.Types.Response
import Estuary.Types.View
import qualified Estuary.Types.EnsembleS as E
import qualified Estuary.Types.Ensemble as E

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

addClient :: ServerState -> UTCTime -> WS.Connection -> IO ClientHandle
addClient s t x = atomically $ do
  oldMap <- readTVar (clients s)
  i <- readTVar (nextClientHandle s)
  c <- newTVar $ newClient t i x
  let newMap = IntMap.insert i c oldMap
  writeTVar (clients s) newMap
  writeTVar (nextClientHandle s) (i+1)
  return i

deleteClient :: ServerState -> ClientHandle -> IO ()
deleteClient s h = atomically $ do
  oldMap <- readTVar (clients s)
  let newMap = IntMap.delete h oldMap
  writeTVar (clients s) newMap

addEnsemble :: ServerState -> Text -> Text -> UTCTime -> IO ()
addEnsemble s name pwd now = atomically $ do
  oldMap <- readTVar (ensembles s)
  newEns <- newTVar $ E.writePassword pwd $ E.emptyEnsembleS now
  let newMap = Map.insertWith (\_ x -> x) name newEns oldMap -- if space already exists, addEnsemble does not make any change
  -- ??? should it perhaps through an exception instead ???
  writeTVar (ensembles s) newMap

{- modifyEnsemble :: ServerState -> Text -> (E.EnsembleS -> E.EnsembleS) -> IO ()
modifyEnsemble s name f = do
  eMap <- atomically $ readTVar (ensembles s)
  let e = Map.lookup name eMap
  case e of
    Just e' -> atomically $ do
      e'' <- readTVar e'
      writeTVar e' (f e'')
    Nothing -> return () -- ??? or should this throw an exception ??? -}
