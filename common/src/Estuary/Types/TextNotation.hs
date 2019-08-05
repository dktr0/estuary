{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.TextNotation where

import GHC.Generics
import Data.Aeson

import Estuary.Types.TidalParser

data TextNotation =
  TidalTextNotation TidalParser |
  Punctual |
  CineCer0 |
  TimeNot
  deriving (Read,Eq,Ord,Show,Generic)

instance ToJSON TextNotation
instance FromJSON TextNotation

textNotationDropDownLabel :: TextNotation -> String
textNotationDropDownLabel (TidalTextNotation x) = show x
textNotationDropDownLabel x = show x
