{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.VideoMeta where

import Data.Text
import GHC.Generics
import Data.Aeson

import Estuary.Types.AspectRatio

data VideoMeta = VideoMeta {
  videoURL :: Text,
  videoDuration :: Double,
  videoResolution :: (Int, Int),
  videoAspectRatio :: AspectRatio
  } deriving (Show,Generic)

instance ToJSON VideoMeta where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON VideoMeta
