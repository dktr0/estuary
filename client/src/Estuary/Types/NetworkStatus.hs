module Estuary.Types.NetworkStatus where

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
  aboutThisServer = Map.fromList [(English,"")], -- non-empty in order to suppress "?"
  announcements = Map.empty
}
