{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.TextNotation where

import Text.JSON
import Text.JSON.Generic

import Estuary.Types.TidalParser

data TextNotation =
  TidalTextNotation TidalParser
  deriving (Show,Read,Eq,Ord,Data,Typeable)

instance JSON TextNotation where
  showJSON = toJSON
  readJSON = fromJSON
