{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.AudioMeta where

import Data.Text
import GHC.Generics
import Data.Aeson

data AudioMeta = AudioMeta {
  audioURL :: Text,
  audioDuration :: Double
  } deriving (Show,Generic,Eq)

instance ToJSON AudioMeta where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON AudioMeta
