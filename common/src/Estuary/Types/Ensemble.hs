{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

-- The type Ensemble represents all information about an ensemble that is shared
-- between a given client and a server, including but not limited to all of the
-- live coding "programs" to be rendered by the client.
--
-- (See also EnsembleS which wraps this type with further information held only by the
-- server and EnsembleC which wraps this type with info known only to the client.)

module Estuary.Types.Ensemble where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap as IntMap

import Data.Time
import Data.Text (Text)
import Control.Applicative
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Witherable as Witherable

import Estuary.Types.Tempo
import Estuary.Types.Definition
import Estuary.Types.View
import Estuary.Types.Chat
import Estuary.Types.Participant
import Estuary.Types.ResourceOp

data Ensemble = Ensemble {
  ensembleName :: Text,
  tempo :: Tempo,
  zones :: IntMap.IntMap Definition,
  views :: Map.Map Text View,
  resourceOps :: [ResourceOp],
  chats :: [Chat],
  participants :: Map.Map Text Participant,
  anonymousParticipants :: Int
  } deriving (Generic,Show)

instance ToJSON Ensemble where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Ensemble where
  parseJSON (Object o) = do
    ensembleName' <- o .: "ensembleName"
    tempo' <- o .: "tempo"
    -- parse map of zones, silently omitting any that fail to parse (in order to allow evolution of Definition type)
    zones' <- o .:? "zones" .!= IntMap.empty -- IntMap Value
    zones'' <- mapM (\v -> (Just <$> parseJSON v) <|> return Nothing) zones' -- IntMap (Maybe Definition)
    let zones''' = Witherable.catMaybes zones'' -- IntMap Definition
    -- parse map of views, silently omitting any that fail to parse (in order to allow evolution of View type)
    views' <- o .:? "views" .!= Map.empty -- Map Text Value
    views'' <- mapM (\v -> (Just <$> parseJSON v) <|> return Nothing) views' -- Map Text (Maybe View)
    let views''' = Witherable.catMaybes views'' -- Map Text View
    -- parse list of resourceOps, silently omitting any that fail to parse (in order to allow evolution of ResourceOp type)
    resourceOps' <- o .:? "resourceOps" .!= []
    resourceOps'' <- mapM (\v -> (Just <$> parseJSON v) <|> return Nothing) resourceOps'
    let resourceOps''' = Witherable.catMaybes resourceOps'' -- [ResourceOp]
    chats' <- o .:? "chats" .!= []
    return $ Ensemble {
      ensembleName = ensembleName',
      tempo = tempo',
      zones = zones''',
      views = views''',
      resourceOps = resourceOps''',
      chats = chats',
      participants = Map.empty,
      anonymousParticipants = 0
    }
  parseJSON invalid = modifyFailure ("parsing Ensemble failed, " ++) $ typeMismatch "Object" invalid

emptyEnsemble :: UTCTime -> Ensemble
emptyEnsemble t = Ensemble {
  ensembleName = "",
  tempo = Tempo { time=t, count=0.0, freq=0.5 },
  zones = IntMap.empty,
  views = Map.empty,
  resourceOps = defaultResourceOps,
  chats = [],
  participants = Map.empty,
  anonymousParticipants = 0
  }

leaveEnsemble :: Ensemble -> Ensemble
leaveEnsemble x = x {
  ensembleName = "",
  zones = IntMap.empty,
  resourceOps = defaultResourceOps
  }

writeEnsembleName :: Text -> Ensemble -> Ensemble
writeEnsembleName t e = e { ensembleName = t }

writeTempo :: Tempo -> Ensemble -> Ensemble
writeTempo t e = e { tempo = t }

writeZone :: Int -> Definition -> Ensemble -> Ensemble
writeZone z d e = e { zones = IntMap.insert z d (zones e) }

writeView :: Text -> View -> Ensemble -> Ensemble
writeView w v e = e { views = Map.insert w v (views e) }

appendChat :: Chat -> Ensemble -> Ensemble
appendChat c e = e { chats = c:(chats e) }

writeParticipant :: Text -> Participant -> Ensemble -> Ensemble
writeParticipant k p e = e { participants = Map.insert k p (participants e) }

deleteParticipant :: Text -> Ensemble -> Ensemble
deleteParticipant k e = e { participants = Map.delete k (participants e) }

writeAnonymousParticipants :: Int -> Ensemble -> Ensemble
writeAnonymousParticipants n e = e { anonymousParticipants = n }
