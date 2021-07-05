module Estuary.Types.ResourceOp where

import Data.Text
import Estuary.Types.ResourceMeta
import Estuary.Types.ResourceType
import Estuary.Types.Location

data ResourceOp =
  InsertResourceMeta ResourceMeta |
  DeleteResource ResourceType Location |
  ResourceListURL Text
  deriving (Show)
