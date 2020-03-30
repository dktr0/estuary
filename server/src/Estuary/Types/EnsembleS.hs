{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- The type EnsembleS represents an Ensemble from the standpoint of the server.

module Estuary.Types.EnsembleS where

import Data.Text (Text)
import Data.Time
import Control.Concurrent.STM
import Data.Map as Map
import Data.IntMap as IntMap

import Estuary.Types.Tempo
import Estuary.Types.Definition
import Estuary.Types.View

data EnsembleS = EnsembleS {
  creationTime :: UTCTime,
  password :: Text,
  expiry :: Maybe NominalDiffTime,
  lastActionTime :: TVar UTCTime,
  tempo :: TVar Tempo,
  zones :: TVar (IntMap.IntMap (TVar Definition)),
  views :: TVar (Map.Map Text (TVar View))
  }

newEnsembleS :: UTCTime -> Text -> STM EnsembleS
newEnsembleS now pwd = do
  tempoTvar <- newTVar $ Tempo {
    cps = 0.5,
    at = now,
    beat = 0
    }
  zonesTvar <- newTVar IntMap.empty
  viewsTvar <- newTVar Map.empty
  lat <- newTVar now
  return $ EnsembleS {
    creationTime = now,
    password = pwd,
    expiry = Just 604800, -- expiry hard-coded at seven days for now
    lastActionTime = lat,
    tempo = tempoTvar,
    zones = zonesTvar,
    views = viewsTvar
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
