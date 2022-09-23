{-# LANGUAGE DeriveGeneric #-}

-- This type represents all messages that an Estuary server can send
-- to an Estuary client via WebSockets.

module Estuary.Types.Response where

import Data.Maybe (mapMaybe,catMaybes)
import Data.Time.Clock
import Data.Text
import GHC.Generics
import Data.Aeson
import Data.Sequence

import Estuary.Utility
import Estuary.Types.Definition
import Estuary.Types.Participant
import Estuary.Types.LogEntry
import Estuary.Types.View
import Estuary.Types.Tempo
import Estuary.Types.ResourceOp

data Response =
  ServerInfo Int UTCTime | -- response to ClientInfo: serverClientCount pingTime (from triggering ClientInfo)
  OK Text | -- eg. ensemble successfully deleted -- could this include the request it was a response to?
  Error Text | -- eg. ensemble login failure -- could this include the request it was a response to?
  EnsembleList [Text] |
  JoinedEnsemble Text Text Text Text | -- ensemble username location password -- could be deleted if ResponseOk includes Request? (see above)
  WriteZone Int Definition |
  LogEntry LogEntry |
  WriteView Text View |
  WriteTempo Tempo |
  ParticipantUpdate Participant | -- could this profitably be Text Participant, with handle removed from Participant type?
  ParticipantLeaves Text |
  AnonymousParticipants Int |
  WriteResourceOps (Seq ResourceOp) |
  ResetZones |
  ResetViews |
  ResetTempo Tempo | -- reset the metric grid/tempo only
  Reset Tempo -- reset the zones, views and metric grid/tempo (with the provided tempo)
  deriving (Generic)

instance ToJSON Response where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Response

{-

all of the definitions below, if still needed, should be in client tree instead:

responsesToEnsembleList :: [Response] -> Maybe [Text]
responsesToEnsembleList = lastOrNothing . mapMaybe f
  where f (EnsembleList x) = Just x
        f _ = Nothing

justJoinedEnsemble :: [Response] -> Maybe (Text,Text,Text,Text)
justJoinedEnsemble = lastOrNothing . mapMaybe f
  where f (JoinedEnsemble a b c d) = Just (a,b,c,d)
        f _ = Nothing

justResponseOK :: [Response] -> Maybe Text
justResponseOK = lastOrNothing . mapMaybe f
  where f (ResponseOK x) = Just x
        f _ = Nothing

justResponseError :: [Response] -> Maybe Text
justResponseError = lastOrNothing . mapMaybe f
  where f (ResponseError x) = Just x
        f _ = Nothing

justServerInfo :: Response -> Maybe (Int,UTCTime)
justServerInfo (ServerInfo x y) = Just (x,y)
justServerInfo _ = Nothing

-}
