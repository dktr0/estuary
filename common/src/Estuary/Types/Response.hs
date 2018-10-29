module Estuary.Types.Response where

import Data.Maybe (mapMaybe)
import Text.JSON
import Estuary.Utility
import Estuary.Types.Sited
import Estuary.Types.EnsembleResponse
import Estuary.Types.Definition

type ServerResponse = Response Definition

data Response a =
  EnsembleList [String] |
  EnsembleResponse (Sited String (EnsembleResponse a)) |
  ServerClientCount Int

instance JSON a => JSON (Response a) where
  showJSON (EnsembleList xs) = encJSDict [("EnsembleList",xs)]
  showJSON (EnsembleResponse r) = encJSDict [("EnsembleResponse",r)]
  showJSON (ServerClientCount r) = encJSDict [("ServerClientCount",r)]
  readJSON (JSObject x) | firstKey x == "EnsembleList" = EnsembleList <$> valFromObj "EnsembleList" x
  readJSON (JSObject x) | firstKey x == "EnsembleResponse" = EnsembleResponse <$> valFromObj "EnsembleResponse" x
  readJSON (JSObject x) | firstKey x == "ServerClientCount" = ServerClientCount <$> valFromObj "ServerClientCount" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as Request: " ++ (show x)
  readJSON _ = Error "Unable to parse as Request"

justEnsembleResponses :: [Response a] -> [Sited String (EnsembleResponse a)]
justEnsembleResponses = mapMaybe f
  where f (EnsembleResponse x) = Just x
        f _ = Nothing

justEnsembleList :: [Response a] -> Maybe [String]
justEnsembleList = lastOrNothing . mapMaybe f
  where f (EnsembleList x) = Just x
        f _ = Nothing

justServerClientCount :: [Response a] -> Maybe Int
justServerClientCount = lastOrNothing . mapMaybe f
  where f (ServerClientCount x) = Just x
        f _ = Nothing

