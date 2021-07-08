{-# LANGUAGE OverloadedStrings, DeriveGeneric, TemplateHaskell #-}

-- The type EnsembleS represents an Ensemble from the standpoint of the server.

module Estuary.Types.EnsembleS where

import Data.Text (Text)
import Data.Time
import Control.Concurrent.STM hiding (atomically,readTVarIO)
import Data.Map as Map
import Data.IntMap as IntMap
import qualified Network.WebSockets as WS
import Control.Monad
import Data.Maybe
import Data.Sequence

import Estuary.Types.Tempo
import Estuary.Types.Definition
import Estuary.Types.View
import Estuary.AtomicallyTimed
import Estuary.Types.Response
import Estuary.Types.ResourceOp

data EnsembleS = EnsembleS {
  ensembleName :: Text,
  creationTime :: UTCTime,
  ownerPassword :: Text, -- used to "self-delete" this ensemble
  joinPassword :: Text, -- used to join an ensemble with editing privileges
  expiry :: Maybe NominalDiffTime,
  lastActionTime :: TVar UTCTime,
  tempo :: TVar Tempo,
  zones :: TVar (IntMap.IntMap (TVar Definition)),
  views :: TVar (Map.Map Text (TVar View)),
  resourceOps :: TVar (Seq ResourceOp),
  connections :: TVar (IntMap.IntMap (TChan (Int,Response))),
  namedConnections :: TVar (IntMap.IntMap Text),
  anonymousConnections :: TVar Int
  }

newEnsembleS :: Text -> UTCTime -> Text -> Text -> Maybe NominalDiffTime -> STM EnsembleS
newEnsembleS eName now opwd jpwd expTime = do
  tempoTvar <- newTVar $ Tempo {
    freq = 0.5,
    time = now,
    count = 0
    }
  zonesTvar <- newTVar IntMap.empty
  viewsTvar <- newTVar Map.empty
  resourceOpsTvar <- newTVar Data.Sequence.empty
  connectionsTvar <- newTVar IntMap.empty
  namedConnectionsTvar <- newTVar IntMap.empty
  anonymousConnectionsTvar <- newTVar 0
  lat <- newTVar now
  return $ EnsembleS {
    ensembleName = eName,
    creationTime = now,
    ownerPassword = opwd,
    joinPassword = jpwd,
    expiry = expTime,
    lastActionTime = lat,
    tempo = tempoTvar,
    zones = zonesTvar,
    views = viewsTvar,
    resourceOps = resourceOpsTvar,
    connections = connectionsTvar,
    namedConnections = namedConnectionsTvar,
    anonymousConnections = anonymousConnectionsTvar
  }

writeTempo :: EnsembleS -> UTCTime -> Tempo -> STM ()
writeTempo e now t = do
  writeTVar (tempo e) t
  writeTVar (lastActionTime e) now

readTempo :: EnsembleS -> STM Tempo
readTempo e = readTVar (tempo e)

writeZone :: EnsembleS -> UTCTime -> Int -> Definition -> STM ()
writeZone e now n d = do
  zs <- readTVar (zones e)
  case (IntMap.lookup n zs) of
    (Just z) -> writeTVar z d
    Nothing -> do
      z <- newTVar d
      writeTVar (zones e) $ IntMap.insert n z zs
  writeTVar (lastActionTime e) now

readZones :: EnsembleS -> STM (IntMap.IntMap Definition)
readZones e = readTVar (zones e) >>= mapM readTVar

resetZones :: EnsembleS -> UTCTime -> STM ()
resetZones e now = do
  writeTVar (zones e) IntMap.empty
  writeTVar (lastActionTime e) now

resetViews :: EnsembleS -> UTCTime -> STM ()
resetViews e now = do
  writeTVar (views e) Map.empty
  writeTVar (lastActionTime e) now

writeView :: EnsembleS -> UTCTime -> Text -> View -> STM ()
writeView e now n v = do
  vs <- readTVar (views e)
  case (Map.lookup n vs) of
    (Just vtvar) -> writeTVar vtvar v
    Nothing -> do
      vtvar <- newTVar v
      writeTVar (views e) $ Map.insert n vtvar vs
  writeTVar (lastActionTime e) now

readViews :: EnsembleS -> STM (Map.Map Text View)
readViews e = readTVar (views e) >>= mapM readTVar

writeResourceOps :: EnsembleS -> UTCTime -> Seq ResourceOp -> STM ()
writeResourceOps e now ops = do
  writeTVar (resourceOps e) ops
  writeTVar (lastActionTime e) now

readResourceOps :: EnsembleS -> STM (Seq ResourceOp)
readResourceOps e = readTVar (resourceOps e)

-- Int argument is ClientHandle
addNamedConnection :: Int -> Text -> TChan (Int,Response) -> EnsembleS -> STM ()
addNamedConnection h n sChan e = do
  oldConnectionsMap <- readTVar $ connections e
  writeTVar (connections e) $ IntMap.insert h sChan oldConnectionsMap
  oldNamesMap <- readTVar $ namedConnections e
  writeTVar (namedConnections e) $ IntMap.insert h n oldNamesMap

-- Int argument is ClientHandle
addAnonymousConnection :: Int -> TChan (Int,Response) -> EnsembleS -> STM ()
addAnonymousConnection h sChan e = do
  oldConnectionsMap <- readTVar $ connections e
  writeTVar (connections e) $ IntMap.insert h sChan oldConnectionsMap
  oldCount <- readTVar $ anonymousConnections e
  writeTVar (anonymousConnections e) $ oldCount + 1

-- Int argument is ClientHandle
deleteConnection :: Int -> EnsembleS -> STM ()
deleteConnection h e = do
  oldConnectionsMap <- readTVar $ connections e
  writeTVar (connections e) $ IntMap.delete h oldConnectionsMap
  oldNamesMap <- readTVar $ namedConnections e
  writeTVar (namedConnections e) $ IntMap.delete h oldNamesMap
  -- if connection was not named, then decrement anonymous count
  when (isNothing $ IntMap.lookup h oldNamesMap) $ do
    oldCount <- readTVar $ anonymousConnections e
    writeTVar (anonymousConnections e) $ oldCount - 1

countAnonymousConnections :: EnsembleS -> STM Int
countAnonymousConnections e = readTVar (anonymousConnections e)

nameTaken :: EnsembleS -> Text -> STM Bool
nameTaken e x = do
  xs <- IntMap.elems <$> readTVar (namedConnections e)
  return $ elem x xs

readConnections :: EnsembleS -> STM (IntMap (TChan (Int,Response)))
readConnections e = readTVar (connections e)

-- Int argument is ClientHandle
readConnectionsNoOrigin :: EnsembleS -> Int -> STM (IntMap (TChan (Int,Response)))
readConnectionsNoOrigin e h = do
  x <- readTVar $ connections e
  return $ IntMap.delete h x
