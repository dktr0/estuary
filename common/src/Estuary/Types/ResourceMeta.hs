module Estuary.Types.ResourceMeta where

-- A ResourceMeta says "This image/exolang/reslist/etc..":
--  is at this URL
--  is nicknamed (located) with this Text and Int (i.e. Location) in some context (eg. a custom sample library, the 
--  may also have a descriptive comment attached to it

import Data.Text

import Estuary.Types.ResourceType
import Estuary.Types.Location

data ResourceMeta = ResourceMeta {
  resourceURL :: Text,
  resourceType :: ResourceType,
  resourceLocation :: Location,
  resourceComment :: Text
  } deriving (Show)
