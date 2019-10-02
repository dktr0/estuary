{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.TextNotation where

import GHC.Generics
import Data.Aeson

import Estuary.Types.TidalParser

data TextNotation =
  TidalTextNotation TidalParser |
  Punctual |
  Escuchar |
  TimeNot |
  Ver |
  Oir |
  Dos
  deriving (Read,Eq,Ord,Show,Generic)

instance ToJSON TextNotation where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TextNotation

textNotationDropDownLabel :: TextNotation -> String
textNotationDropDownLabel (TidalTextNotation x) = show x
textNotationDropDownLabel x = show x
