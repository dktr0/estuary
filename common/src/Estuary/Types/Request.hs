{-# LANGUAGE DeriveGeneric #-}

-- This type represents all messages that an Estuary client can send
-- to an Estuary server via WebSockets.

module Estuary.Types.Request where

import Data.Time.Clock
import Data.Text
import GHC.Generics
import Data.Aeson

import Estuary.Types.EnsembleRequest
import Estuary.Types.Definition

data Request =
  BrowserInfo Text | -- text is browser userAgent field, issued at client launch (by alternateWebSocket)
  ClientInfo Int Int Int NominalDiffTime UTCTime | -- load animationFPS animationLoad serverLatency pingTime, issued every 5s (by alternateWebSocket)
  GetEnsembleList | -- issued when client enters Lobby page
  JoinEnsemble Text Text Text Text | -- ensemble username location password (username, location, and password can be "")
  RejoinEnsemble Text Text Text Text | -- for re-authenticating when websocket connections close and re-open
  EnsembleRequest EnsembleRequest | -- see Estuary.Types.EnsembleRequest, request "within" an ensemble
  LeaveEnsemble |
  CreateEnsemble Text Text Text Text (Maybe NominalDiffTime) | -- communityPassword ensembleName ownerPassword joinerPassword expiryTime (empty for no password)
  DeleteThisEnsemble Text | -- ownerPassword
  DeleteEnsemble Text Text -- ensembleName moderatorPassword
  deriving (Generic)

instance ToJSON Request where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Request
