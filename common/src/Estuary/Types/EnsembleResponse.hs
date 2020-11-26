{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.EnsembleResponse where

import Data.Time
import Data.Text (Text)
import Data.Maybe (mapMaybe,listToMaybe)
import GHC.Generics
import Data.Aeson

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
  ParticipantJoins Participant |
  ParticipantUpdate Participant |
  ParticipantLeaves Text |
  AnonymousParticipants Int |
  ResetZonesResponse |
  ResetViewsResponse |
  ResetTempoResponse Tempo |
  ResetResponse Tempo
  deriving (Generic,Show)

instance ToJSON EnsembleResponse where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON EnsembleResponse

lastEditOrResetInZone :: Definition -> Int -> [EnsembleResponse] -> Maybe Definition
lastEditOrResetInZone resetValue z = g . mapMaybe f
  where
    f (ZoneRcvd z2 x) | z==z2 = Just x
    f ResetZonesResponse = Just resetValue
    f (ResetResponse _) = Just resetValue
    f _ = Nothing
    g [] = Nothing
    g xs = Just $ last xs

-- deprecated: maybe not used any more?
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
