{-# LANGUAGE DeriveGeneric #-}

-- note: this is being reworked so that it only serves the strict purpose of communication
-- between the UI and the rendering engine. As such, it will move from the common tree to the client tree.

module Estuary.Types.EnsembleOp where

import Data.Time
import Data.Text
import GHC.Generics
import Data.Aeson
import Data.Sequence

import Estuary.Types.View
import Estuary.Types.Definition
import Estuary.Types.Tempo
import Estuary.Types.ResourceOp
import Estuary.Types.Chat

-- EnsembleOp-s can come from the local user or the remote server (in ensembles)
-- and may (or may not) have an impact on rendering. Thus, this type is used by all
-- three of the UI, the rendering engine, and the server. (The EnsembleOp type is
-- the only representation the UI uses to send changes to an Ensemble to the server and
-- to the rendering engine.)

data EnsembleOp =
  WriteTempo Tempo |
  WriteZone Int Definition |
  WriteView Text View |
  WriteChat Chat |
  WriteStatus Text |
  WriteResourceOps (Seq ResourceOp) |
  ResetZones |
  ResetViews |
  ResetTempo Tempo | -- reset the metric grid/tempo only
  Reset Tempo -- reset the zones, views and metric grid/tempo (with the provided tempo)
  deriving (Generic)

instance ToJSON EnsembleOp where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON EnsembleOp
