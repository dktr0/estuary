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
import Estuary.Types.EnsembleEvent

data Response =
  ServerInfo Int UTCTime | -- response to ClientInfo: serverClientCount pingTime
  ResponseOK Text | -- eg. ensemble successfully deleted
  ResponseError Text | -- eg. ensemble login failure
  EnsembleList [Text] |
  EnsembleResponse EnsembleEvent
  deriving (Generic)

instance ToJSON Response where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Response

justEnsembleResponses :: [Response] -> [EnsembleEvent]
justEnsembleResponses = mapMaybe f
  where f (EnsembleResponse x) = Just x
        f _ = Nothing

justEnsembleList :: [Response] -> Maybe [Text]
justEnsembleList = lastOrNothing . mapMaybe f
  where f (EnsembleList x) = Just x
        f _ = Nothing

justResponseOK :: [Response] -> Maybe Text
justResponseOK = lastOrNothing . mapMaybe f
  where f (ResponseOK x) = Just x
        f _ = Nothing

justResponseError :: [Response] -> Maybe Text
justResponseError = lastOrNothing . mapMaybe f
  where f (ResponseError x) = Just x
        f _ = Nothing

justServerInfo :: Response -> Maybe (Int,UTCTime)
justServerInfo (ServerInfo x y) = Just (x,y)
justServerInfo _ = Nothing
