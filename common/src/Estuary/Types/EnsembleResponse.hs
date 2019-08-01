{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.EnsembleResponse where

import Text.JSON
import Text.JSON.Generic
import Data.Time
import Data.Text (Text)
import Data.Maybe (mapMaybe,listToMaybe)

import Estuary.Types.View
import Estuary.Types.Tempo
import Estuary.Types.Definition
import Estuary.Types.Participant
import Estuary.Types.Chat

data EnsembleResponse =
  TempoRcvd Tempo |
  ZoneRcvd Int Definition |
  ViewRcvd Text View |
  ChatRcvd Chat |
  ParticipantJoins Text Participant |
  ParticipantUpdate Text Participant |
  ParticipantLeaves Text |
  AnonymousParticipants Int
  deriving (Data,Typeable)

instance JSON EnsembleResponse where
  showJSON = toJSON
  readJSON = fromJSON

justEditsInZone :: Int -> [EnsembleResponse] -> [Definition]
justEditsInZone z1 = mapMaybe f
  where
    f (ZoneRcvd z2 a) | z1==z2 = Just a
    f _ = Nothing

justChats :: [EnsembleResponse] -> [Chat]
justChats = mapMaybe f
  where f (ChatRcvd x) = Just x
        f _ = Nothing

justViews :: [EnsembleResponse] -> [(Text,View)]
justViews = mapMaybe f
  where f (ViewRcvd x y) = Just (x,y)
        f _ = Nothing

lastTempoChange :: [EnsembleResponse] -> Maybe Tempo
lastTempoChange = listToMaybe . reverse . mapMaybe f
  where f (TempoRcvd theTempo) = Just theTempo
        f _ = Nothing
