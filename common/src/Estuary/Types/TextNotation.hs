{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.TextNotation where

import Text.JSON
import Text.JSON.Generic

import Estuary.Types.TidalParser

data TextNotation =
  TidalTextNotation TidalParser |
  PunctualAudio |
  PunctualVideo |
  SvgOp |
  CanvasOp
  deriving (Read,Eq,Ord,Data,Typeable)

instance Show TextNotation where
  show (TidalTextNotation x) = show x
  show PunctualAudio = "PunctualAudio"
  show PunctualVideo = "PunctualVideo"
  show SvgOp = "SvgOp"
  show CanvasOp = "CanvasOp"

instance JSON TextNotation where
  showJSON = toJSON
  readJSON = fromJSON
