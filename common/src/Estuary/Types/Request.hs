{-# LANGUAGE DeriveGeneric #-}

-- This type represents all messages that an Estuary client can send
-- to an Estuary server via WebSockets.

module Estuary.Types.Request where

import Data.Time.Clock
import Data.Text
import GHC.Generics
import Data.Aeson

import Estuary.Types.EnsembleEvent

data Request =
  BrowserInfo Text | -- text is browser userAgent field, issued at client launch (by alternateWebSocket)
  ClientInfo Int Int Int NominalDiffTime UTCTime | -- load animationFPS animationLoad serverLatency pingTime, issued every 5s (by alternateWebSocket)
  GetEnsembleList | -- issued when client enters Lobby page
  EnsembleRequest EnsembleEvent | -- see Estuary.Types.EnsembleEvent, request "within" an ensemble
  CreateEnsemble Text Text Text Text (Maybe NominalDiffTime) | -- communityPassword ensembleName ownerPassword joinerPassword expiryTime (empty for no password)
  DeleteEnsemble Text Text -- ensembleName moderatorPassword
  deriving (Eq,Generic)

instance ToJSON Request where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Request
