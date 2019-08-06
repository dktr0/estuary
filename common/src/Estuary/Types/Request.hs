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
  ClientInfo UTCTime Int Int NominalDiffTime | -- pingTime load animationLoad serverLatency, issued every 5s (by alternateWebSocket)
  GetEnsembleList | -- issued when client enters Lobby page
  JoinEnsemble Text Text Text Text | -- ensemble username location password (username, location, and password can be "")
  EnsembleRequest EnsembleRequest | -- see Estuary.Types.EnsembleRequest, request "within" an ensemble
  LeaveEnsemble |
  Authenticate Text | -- ie. as administrator, for making ensembles
  CreateEnsemble Text Text -- ensemble password (empty for no password)
  deriving (Eq,Generic)

instance ToJSON Request where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Request
