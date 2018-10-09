{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.TextNotation where

import Text.JSON
import Text.JSON.Generic

import Estuary.Languages.TidalParser

data TextNotation =
  TidalTextNotation TidalParser
  deriving (Show,Eq,Data,Typeable)

instance JSON TextNotation where
  showJSON = toJSON
  readJSON = fromJSON
