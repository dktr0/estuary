{-# LANGUAGE DeriveDataTypeable #-}

-- This type represents all messages that an Estuary client can send
-- to an Estuary server via WebSockets.

module Estuary.Types.Request where

import Text.JSON
import Text.JSON.Generic
import Data.Time.Clock
import Data.Text

import Estuary.Types.Sited
import Estuary.Types.EnsembleRequest
import Estuary.Types.Definition

data Request =
  BrowserInfo Text | -- issued once at Estuary launch, identifies name/version of browser
  ClientInfo UTCTime Double Double NominalDiffTime | -- pingTime load animationLoad
  GetEnsembleList |
  JoinEnsemble Text Text Text (Maybe Text) | -- ensemble username location password (username and location can be "")
  EnsembleRequest EnsembleRequest | -- see Estuary.Types.EnsembleRequest, request "within" an ensemble
  LeaveEnsemble |
  Authenticate Text | -- ie. as administrator, for making ensembles
  CreateEnsemble Text Text -- ensemble password (empty for no password)
  deriving (Eq,Data,Typeable)

instance JSON Request where
  showJSON = toJSON
  readJSON = fromJSON
