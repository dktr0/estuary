{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.ResourceType where

import GHC.Generics
import Data.Aeson

data ResourceType = Audio | Image | Video | ExoLang | ResList | GLTF deriving (Show, Eq, Generic)

instance ToJSON ResourceType where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ResourceType
