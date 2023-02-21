{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Estuary.Types.LogEntry where

import Data.Time.Clock
import Data.Text
import GHC.Generics
import Data.Aeson

import Estuary.Types.Language
import Estuary.Types.TranslatableText

data LogEntry = LogEntry {
  logEntryTime :: UTCTime,
  logEntrySender :: Text, -- empty for everything except chat messages
  logEntryText :: TranslatableText
  }
  deriving (Generic,Eq,Ord)

showtLogEntry :: Language -> LogEntry -> Text
showtLogEntry l x
  | logEntrySender x == "" = translateText (logEntryText x) l
  | otherwise = logEntrySender x <> ": " <> translateText (logEntryText x) l

instance ToJSON LogEntry where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON LogEntry
