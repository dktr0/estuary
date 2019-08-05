{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.EnsembleRequest where

import Data.Time
import Data.Text
import GHC.Generics
import Data.Aeson

import Estuary.Types.View
import Estuary.Types.Definition
import Estuary.Types.Tempo

data EnsembleRequest =
  WriteTempo Tempo |
  WriteZone Int Definition |
  WriteView Text View |
  WriteChat Text |
  WriteStatus Text
  deriving (Eq,Generic)

instance ToJSON EnsembleRequest
instance FromJSON EnsembleRequest
