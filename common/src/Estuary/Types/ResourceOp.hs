module Estuary.Types.ResourceOp where

import Data.Text
import Estuary.Types.ResourceMeta

data ResourceOp =
  InsertResourceMeta ResourceMeta |
  ResourceListURL Text
  deriving (Show)
