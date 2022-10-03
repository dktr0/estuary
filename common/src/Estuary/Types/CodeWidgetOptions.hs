{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Estuary.Types.CodeWidgetOptions where

import GHC.Generics
import Data.Aeson


data CodeWidgetOptions =
  Nomenu | -- "nomenu"
  Noeval | -- "noeval"
  Noerrors | -- "noerrors"
  Fluxus | -- "fluxus"
  CentreAlign | -- "centre" or "center"
  RightAlign | -- "right"
  Fontsize Double -- "fontsize 2"
  deriving (Show, Eq, Generic)


instance ToJSON CodeWidgetOptions where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON CodeWidgetOptions
