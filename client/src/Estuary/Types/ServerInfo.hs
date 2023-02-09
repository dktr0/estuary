{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.ServerInfo where

import Data.Map as Map
import Data.Text
import Data.Time

import Estuary.Types.TranslatableText
import Estuary.Types.Language

data ServerInfo = ServerInfo {
  aboutThisServer :: TranslatableText,
  announcements :: Map Day [TranslatableText],
  wsStatus :: Text,
  serverLatency :: NominalDiffTime,
  clientCount :: Int
  }

emptyServerInfo :: ServerInfo
emptyServerInfo = ServerInfo {
  aboutThisServer = Map.fromList [(English,"")], -- non-empty in order to suppress "?"
  announcements = Map.empty,
  wsStatus = "",
  serverLatency = 0,
  clientCount = 0
  }
