{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.EnsembleRequest where

import Text.JSON
import Text.JSON.Generic

import Estuary.Types.View
import Estuary.Types.Definition
import Estuary.Types.Tempo
import Data.Time
import Data.Text

data EnsembleRequest =
  ZoneRequest Int Definition |
  SendChat Text |
  SendStatus Text |
  PublishView Text View |
  PublishDefaultView View |
  DeleteView Text |
  SetTempo Tempo
  deriving (Eq,Data,Typeable)

instance JSON EnsembleRequest where
  showJSON = toJSON
  readJSON = fromJSON
