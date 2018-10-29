{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.EnsembleRequest where

import Text.JSON
import Text.JSON.Generic

import Estuary.Types.Sited
import Estuary.Types.EditOrEval
import Estuary.Types.View
import Estuary.Types.Definition
import Estuary.Types.Tempo

data EnsembleRequest =
  AuthenticateInEnsemble String |
  SendChat String String | -- name message
  ZoneRequest (Sited Int (EditOrEval Definition)) |
  ListViews |
  GetView String |
  PublishView (Sited String View) |
  PublishDefaultView View |
  DeleteView String |
  SetTempo Tempo |
  GetEnsembleClientCount
  deriving (Eq,Data,Typeable)

instance JSON EnsembleRequest where
  showJSON = toJSON
  readJSON = fromJSON
