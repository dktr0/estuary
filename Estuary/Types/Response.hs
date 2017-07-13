module Estuary.Types.Response where

import Data.Maybe (mapMaybe)
import Text.JSON
import Estuary.Utility
import Estuary.Types.Sited
import Estuary.Types.SpaceResponse
import Estuary.Types.Definition

type ServerResponse = Response Definition

data Response a =
  SpaceList [String] |
  SpaceResponse (Sited String (SpaceResponse a)) |
  ServerClientCount Int

instance JSON a => JSON (Response a) where
  showJSON (SpaceList xs) = encJSDict [("SpaceList",xs)]
  showJSON (SpaceResponse r) = encJSDict [("SpaceResponse",r)]
  showJSON (ServerClientCount r) = encJSDict [("ServerClientCount",r)]
  readJSON (JSObject x) | firstKey x == "SpaceList" = SpaceList <$> valFromObj "SpaceList" x
  readJSON (JSObject x) | firstKey x == "SpaceResponse" = SpaceResponse <$> valFromObj "SpaceResponse" x
  readJSON (JSObject x) | firstKey x == "ServerClientCount" = ServerClientCount <$> valFromObj "ServerClientCount" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as Request: " ++ (show x)
  readJSON _ = Error "Unable to parse as Request"

justSpaceResponses :: [Response a] -> [Sited String (SpaceResponse a)]
justSpaceResponses = mapMaybe f
  where f (SpaceResponse x) = Just x
        f _ = Nothing

justSpaceList :: [Response a] -> Maybe [String]
justSpaceList = lastOrNothing . mapMaybe f
  where f (SpaceList x) = Just x
        f _ = Nothing

justServerClientCount :: [Response a] -> Maybe Int
justServerClientCount = lastOrNothing . mapMaybe f
  where f (ServerClientCount x) = Just x
        f _ = Nothing

