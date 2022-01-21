{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.TidalParser where

import GHC.Generics
import Data.Aeson

data TidalParser = MiniTidal
  deriving (Show,Read,Eq,Ord,Generic)

instance ToJSON TidalParser where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TidalParser
