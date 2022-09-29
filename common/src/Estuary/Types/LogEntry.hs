{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.LogEntry where

import Data.Time.Clock
import Data.Text
import GHC.Generics
import Data.Aeson

import Estuary.Types.TranslatableText

data LogEntry = LogEntry {
  logEntryTime :: UTCTime,
  logEntrySender :: Text, -- empty for everything except chat messages
  logEntryText :: TranslatableText
  }
  deriving (Generic,Eq,Ord)

instance ToJSON LogEntry where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON LogEntry
