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
  InsertResource ResourceType Text Location | -- the resource at the URL (Text) will be located at the specified Location (bankName, number) in the bank
  AppendResource ResourceType Text Text | -- the resource at the URL (first Text) will be located at the next available index in the specified bank (second Text)
  DeleteResource ResourceType Location | -- the link between the the location and its resource should be severed immediately (the resource itself may persist)
  PreloadResource ResourceType Location | -- the indicated resource should be loaded immediately
  PreloadBank ResourceType Text | -- all of the resources in the indicated bank should be loaded immediately
  ResourceListURL Text -- ??? is this op no longer necessary if a reslist is a resource type that can be inserted/appended/deleted with the ops above?
  deriving (Show,Generic,Eq)

instance ToJSON ResourceOp where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ResourceOp

defaultResourceOps :: Seq ResourceOp
defaultResourceOps = Data.Sequence.singleton $ ResourceListURL "samples/resources.json"

