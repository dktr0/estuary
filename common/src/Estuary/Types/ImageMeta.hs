{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.ImageMeta where

import Data.Text
import GHC.Generics
import Data.Aeson

import Estuary.Types.AspectRatio

data ImageMeta = ImageMeta {
  imageResolution :: (Int, Int),
  imageAspectRatio :: AspectRatio
  } deriving (Show,Generic)

instance ToJSON ImageMeta where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ImageMeta
