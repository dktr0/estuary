{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.EnsembleEvent where

import Data.Time
import Data.Text
import GHC.Generics
import Data.Aeson

import Estuary.Types.View
import Estuary.Types.Definition
import Estuary.Types.Tempo
import Estuary.Types.Chat

data EnsembleEvent =
  TempoEvent Tempo |
  ZoneEvent Int Definition |
  ClearZones |
  ViewsEvent Text View | -- a named view is published
  ViewEvent (Either View Text) | -- a local view is changed to a custom or preset View
  ChatEvent Chat |
  StatusEvent Text Text |
  JoinEvent Text Text Text Text | -- ensemble username location password
  LeaveEvent |
  InsertAudioResource Text Text Int | -- "url" [bankName] [n]
  DeleteAudioResource Text Int | -- [bankName] [n]
  AppendAudioResource Text Text | -- "url" [bankName]
  ParticipantJoins Participant |
  ParticipantUpdate Participant |
  ParticipantLeaves Text |
  AnonymousParticipants Int |
  DeleteThisEnsemble
  deriving (Eq,Generic)


instance ToJSON EnsembleEvent where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON EnsembleEvent
