{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Estuary.Types.ResourceOp where

import Data.Text as T
import GHC.Generics
import Data.Aeson
import Data.Sequence
import Data.Foldable
import TextShow

import Estuary.Types.ResourceType
import Estuary.Types.Location

data ResourceOp =
  InsertResource ResourceType Text Location | -- Text is URL
  AppendResource ResourceType Text Text | -- first Text is URL, second Text is bankName
  DeleteResource ResourceType Location |
  ResourceListURL Text
  deriving (Show,Generic,Eq)

instance ToJSON ResourceOp where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ResourceOp

defaultResourceOps :: Seq ResourceOp
defaultResourceOps = Data.Sequence.singleton $ ResourceListURL "samples/resources.json"

showResourceOps :: Seq ResourceOp -> Text
showResourceOps xs | Data.Sequence.length xs == 0 = "empty resource ops"
                   | otherwise = T.intercalate ", " $ toList $ mapWithIndex f xs
  where f i x = showt i <> ": " <> showResourceOp x

showResourceOp :: ResourceOp -> Text
showResourceOp (InsertResource Audio url (bankName,n)) = "insertsound " <> showt url <> " " <> bankName <> " " <> showt n
showResourceOp (InsertResource Image url (bankName,n)) = "insertimage " <> showt url <> " " <> bankName <> " " <> showt n
showResourceOp (InsertResource Video url (bankName,n)) = "insertvideo " <> showt url <> " " <> bankName <> " " <> showt n
showResourceOp (AppendResource Audio url bankName) = "appendsound " <> showt url <> " " <> bankName
showResourceOp (AppendResource Image url bankName) = "appendimage " <> showt url <> " " <> bankName
showResourceOp (AppendResource Video url bankName) = "appendvideo " <> showt url <> " " <> bankName
showResourceOp (DeleteResource Audio (bankName,n)) = "deletesound " <> bankName <> " " <> showt n
showResourceOp (DeleteResource Image (bankName,n)) = "deleteimage " <> bankName <> " " <> showt n
showResourceOp (DeleteResource Video (bankName,n)) = "deletevideo " <> bankName <> " " <> showt n
showResourceOp (ResourceListURL url) = "reslist " <> showt url
