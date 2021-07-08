{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.EnsembleRequest where

import Data.Time
import Data.Text
import GHC.Generics
import Data.Aeson
import Data.Sequence

import Estuary.Types.View
import Estuary.Types.Definition
import Estuary.Types.Tempo
import Estuary.Types.ResourceOp

data EnsembleRequest =
  WriteTempo Tempo |
  WriteZone Int Definition |
  WriteView Text View |
  WriteChat Text |
  WriteStatus Text |
  WriteResourceOps (Seq ResourceOp) |
  ResetZonesRequest |
  ResetViewsRequest |
  ResetTempoRequest Tempo |
  ResetRequest Tempo
  deriving (Generic)

instance ToJSON EnsembleRequest where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON EnsembleRequest
