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
  WriteZone Int Definition Bool | -- Bool represents whether new definition should be delivered to renderer
  WriteView Text View |
  SendChat Text |
  WriteStatus Text |
  WriteResourceOps (Seq ResourceOp) |
  ResetZones |
  ResetViews |
  Reset Tempo -- reset the zones, views and metric grid/tempo (with the provided tempo)
  deriving (Generic,Show)

instance ToJSON Request where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Request

requestsForServer :: Bool -> [Request] -> [Request]
requestsForServer inAnEnsemble = Prelude.filter (f inAnEnsemble)
  where
    f False (DeleteThisEnsemble _) = False
    f False (WriteTempo _) = False
    f False (WriteZone _ _ _) = False
    f False (WriteView _ _) = False
    f False (SendChat _) = False
    f False (WriteStatus _) = False
    f False (WriteResourceOps _) = False
    f False ResetZones = False
    f False ResetViews = False
    f False (Reset _) = False
    f _ x = True
