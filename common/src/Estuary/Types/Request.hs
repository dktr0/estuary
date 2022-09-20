{-# LANGUAGE DeriveGeneric #-}

-- This type represents all messages that an Estuary client can send
-- to an Estuary server via WebSockets. This also includes

module Estuary.Types.Request where

import Data.Time.Clock
import Data.Text
import GHC.Generics
import Data.Aeson
import Data.Maybe (mapMaybe)
import Data.Sequence

import Estuary.Types.Definition
import Estuary.Types.Tempo
import Estuary.Types.View
import Estuary.Types.ResourceOp

data Request =
  BrowserInfo Text | -- text is browser userAgent field, issued at client launch (by alternateWebSocket)
  ClientInfo Int Int Int NominalDiffTime UTCTime | -- load animationFPS animationLoad serverLatency pingTime, issued every 5s (by alternateWebSocket)
  GetEnsembleList | -- issued when client enters Lobby page
  CreateEnsemble Text Text Text Text (Maybe NominalDiffTime) | -- communityPassword ensembleName ownerPassword joinerPassword expiryTime (empty for no password)
  JoinEnsemble Text Text Text Text | -- ensemble username location password (username, location, and password can be "")
  RejoinEnsemble Text Text Text Text | -- for re-authenticating when websocket connections close and re-open
  LeaveEnsemble |
  DeleteThisEnsemble Text | -- ownerPassword
  DeleteEnsemble Text Text | -- ensembleName moderatorPassword
  WriteTempo Tempo |
  WriteZone Int Definition |
  WriteView Text View |
  SendChat Text |
  WriteStatus Text |
  WriteResourceOps (Seq ResourceOp) |
  ResetZones |
  ResetViews |
  Reset Tempo -- reset the zones, views and metric grid/tempo (with the provided tempo)
  deriving (Generic)

instance ToJSON Request where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Request
