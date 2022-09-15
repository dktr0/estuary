{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.LogEntry where

import Data.Maybe (mapMaybe,catMaybes)
import Data.Time.Clock
import Data.Text
import GHC.Generics
import Data.Aeson

import Estuary.Utility
import Estuary.Types.Definition
import Estuary.Types.Participant
import Estuary.Types.Chat
import Estuary.Types.TranslatableText

data LogEntry =
  LogChat Chat |
  EnsembleEvent UTCTime TranslatableText -- eg. "so-and-so has joined the ensemble"
  deriving (Generic)

instance ToJSON LogEntry where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON LogEntry
