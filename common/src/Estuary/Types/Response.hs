{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.Response where

import Data.Maybe (mapMaybe)
import Text.JSON
import Text.JSON.Generic
import Data.Time.Clock

import Estuary.Utility
import Estuary.Types.Sited
import Estuary.Types.EnsembleResponse
import Estuary.Types.Definition

data Response =
  EnsembleList [String] |
  EnsembleResponse EnsembleResponse |
  ServerClientCount Int |
  Pong UTCTime
  deriving (Data,Typeable)

instance JSON Response where
  showJSON = toJSON
  readJSON = fromJSON

justEnsembleResponses :: [Response] -> [EnsembleResponse]
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

justPongs :: [Response] -> Maybe UTCTime
justPongs = lastOrNothing . mapMaybe f
  where f (Pong t) = Just t
        f _ = Nothing

