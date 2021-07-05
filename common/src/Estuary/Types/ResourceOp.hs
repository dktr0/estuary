module Estuary.Types.ResourceOp where

import Data.Text
import Estuary.Types.ResourceMeta
import Estuary.Types.ResourceType
import Estuary.Types.Location

data ResourceOp =
  InsertResource ResourceType Text Location | -- Text is URL
  AppendResource ResourceType Text Text | -- first Text is URL, second Text is bankName
  DeleteResource ResourceType Location |
  ResourceListURL Text
  deriving (Show)
