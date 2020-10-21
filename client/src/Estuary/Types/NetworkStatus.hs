{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.NetworkStatus where

import Data.Text
import Data.Time
import Estuary.Types.Language
import Estuary.Types.TranslatableText
import Data.Map

data NetworkStatus = NetworkStatus {
  wsStatus :: Text,
  serverLatency :: NominalDiffTime,
  clientCount :: Int,
  aboutThisServer :: TranslatableText,
  announcements :: Map Day [TranslatableText]
}

defaultNetworkStatus :: NetworkStatus
defaultNetworkStatus = NetworkStatus {
  wsStatus = "",
  serverLatency = 0,
  clientCount = 0,
  aboutThisServer = fromList [(English,"")], -- non-empty in order to suppress "?"
  announcements = Data.Map.empty
}
