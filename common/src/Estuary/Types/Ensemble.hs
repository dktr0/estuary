{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

-- The type Ensemble represents all information about an ensemble that is shared
-- between a given client and a server, including but not limited to all of the
-- live coding "programs" to be rendered by the client.
--
-- (See also EnsembleS which wraps this type with further information held only by the
-- server and EnsembleB which wraps this type with info known only to the client.)

module Estuary.Types.Ensemble where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap as IntMap

import Data.Time
import Text.JSON
import Text.JSON.Generic
import Data.Text (Text)
import Control.Applicative

import Estuary.Types.Tempo
import Estuary.Types.Definition
import Estuary.Types.View
import Estuary.Types.Chat
import Estuary.Types.Participant

data Ensemble = Ensemble {
  ensembleName :: Text,
  tempo :: Tempo,
  zones :: IntMap.IntMap Definition,
  views :: Map.Map Text View,
  chats :: [Chat],
  participants :: Map.Map Text Participant,
  anonymousParticipants :: Int
  } deriving (Data,Typeable)

instance JSON Ensemble where
  showJSON = toJSON
  readJSON = fromJSON

emptyEnsemble :: UTCTime -> Ensemble
emptyEnsemble t = Ensemble {
  ensembleName = "",
  tempo = Tempo { at=t, beat=0.0, cps=0.5 },
  zones = IntMap.empty,
  views = Map.empty,
  chats = [],
  participants = Map.empty,
  anonymousParticipants = 0
  }

leaveEnsemble :: Ensemble -> Ensemble
leaveEnsemble x = x {
  ensembleName = "",
  zones = IntMap.empty
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
