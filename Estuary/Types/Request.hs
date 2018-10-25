{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.Request where

import Text.JSON
import Text.JSON.Generic

import Estuary.Types.Sited
import Estuary.Types.EnsembleRequest
import Estuary.Types.Definition

data Request =
  Authenticate String |
  GetEnsembleList |
  JoinEnsemble String |
  LeaveEnsemble |
  CreateEnsemble String String | -- ensembleName ensemblePassword (or "" for no password)
  EnsembleRequest (Sited String EnsembleRequest) |
  GetServerClientCount
  deriving (Eq,Data,Typeable)

instance JSON Request where
  showJSON = toJSON
  readJSON = fromJSON
