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
  deriving (Generic,Eq)

instance ToJSON LogEntry where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON LogEntry

instance Ord LogEntry where
  compare (LogChat c1) (LogChat c2) = compare (chatTime c1) (chatTime c2)
  compare (EnsembleEvent t1 _) (EnsembleEvent t2 _) = compare t1 t2
  compare (LogChat c) (EnsembleEvent t _) = compare (chatTime c) t
  compare (EnsembleEvent t _) (LogChat c) = compare t (chatTime c)
  
