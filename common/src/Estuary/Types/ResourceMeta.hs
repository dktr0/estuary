module Estuary.Types.ResourceMeta where

import Data.Text

import Estuary.Types.ResourceType
import Estuary.Types.Location

data ResourceMeta = ResourceMeta {
  resourceURL :: Text,
  resourceType :: ResourceType,
  resourceLocation :: Location
  } deriving (Show)
