{-# LANGUAGE DeriveGeneric #-}

-- This type represents all messages that an Estuary server can send
-- to an Estuary client via WebSockets.

module Estuary.Types.Response where

import Data.Maybe (mapMaybe)
import Data.Time.Clock
import Data.Text
import GHC.Generics
import Data.Aeson

import Estuary.Utility
import Estuary.Types.EnsembleResponse
import Estuary.Types.Definition

data Response =
  ResponseError Text | -- eg. ensemble login failure
  EnsembleList [Text] |
  JoinedEnsemble Text Text | -- ensemble username
  EnsembleResponse EnsembleResponse |
  ServerInfo Int UTCTime -- response to ClientInfo: serverClientCount pingTime (from triggering ClientInfo)
  deriving (Generic)

instance ToJSON Response where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Response

justEnsembleResponses :: [Response] -> [EnsembleResponse]
justEnsembleResponses = mapMaybe f
  where f (EnsembleResponse x) = Just x
        f _ = Nothing

justEnsembleList :: [Response] -> Maybe [Text]
justEnsembleList = lastOrNothing . mapMaybe f
  where f (EnsembleList x) = Just x
        f _ = Nothing

justJoinedEnsemble :: [Response] -> Maybe (Text,Text)
justJoinedEnsemble = lastOrNothing . mapMaybe f
  where f (JoinedEnsemble x y) = Just (x,y)
        f _ = Nothing

justResponseError :: [Response] -> Maybe Text
justResponseError = lastOrNothing . mapMaybe f
  where f (ResponseError x) = Just x
        f _ = Nothing

justServerInfo :: Response -> Maybe (Int,UTCTime)
justServerInfo (ServerInfo x y) = Just (x,y)
justServerInfo _ = Nothing
