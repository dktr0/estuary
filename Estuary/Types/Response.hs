module Estuary.Types.Response where

import Data.Maybe (mapMaybe)
import Text.JSON
import Estuary.Utility (firstKey)
import Estuary.Types.Sited
import Estuary.Types.Action
import Estuary.Types.Definition

type ServerResponse = Response Definition

data Response a =
  SpaceList [String] |
  SpaceResponse (Sited String (Action a))

instance JSON a => JSON (Response a) where
  showJSON (SpaceList xs) = encJSDict [("SpaceList",showJSON xs)]
  showJSON (SpaceResponse r) = encJSDict [("SpaceResponse",showJSON r)]
  readJSON (JSObject x) | firstKey x == "SpaceList" = SpaceList <$> valFromObj "SpaceList" x
  readJSON (JSObject x) | firstKey x == "SpaceResponse" = SpaceResponse <$> valFromObj "SpaceResponse" x
  readJSON _ = Error "Unable to parse as Estuary.Protocol.JSON.Request"

justSpaceResponses :: [Response a] -> [Sited String (Action a)]
justSpaceResponses = mapMaybe f
  where f (SpaceResponse x) = Just x
        f _ = Nothing

justSpaceList :: [Response a] -> [String]
justSpaceList = g . mapMaybe f
  where f (SpaceList x) = Just x
        f _ = Nothing
        g [] = []
        g (x:xs) = x
