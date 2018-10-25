{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.Response where

import Data.Maybe (mapMaybe)
import Text.JSON
import Text.JSON.Generic

import Estuary.Utility
import Estuary.Types.Sited
import Estuary.Types.EnsembleResponse
import Estuary.Types.Definition

data Response =
  EnsembleList [String] |
  EnsembleResponse (Sited String EnsembleResponse) |
  ServerClientCount Int
  deriving (Data,Typeable)

instance JSON Response where
  showJSON = toJSON
  readJSON = fromJSON

justEnsembleResponses :: [Response] -> [Sited String EnsembleResponse]
justEnsembleResponses = mapMaybe f
  where f (EnsembleResponse x) = Just x
        f _ = Nothing

justEnsembleList :: [Response] -> Maybe [String]
justEnsembleList = lastOrNothing . mapMaybe f
  where f (EnsembleList x) = Just x
        f _ = Nothing

justServerClientCount :: [Response] -> Maybe Int
justServerClientCount = lastOrNothing . mapMaybe f
  where f (ServerClientCount x) = Just x
        f _ = Nothing
