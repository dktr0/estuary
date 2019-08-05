{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Estuary.Types.View where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson

data View =
  Views [View] |
  ViewDiv Text View |
  LabelView Int |
  StructureView Int |
  TextView Int Int | -- first int is zone to edit, second int is number of lines in editor
  SequenceView Int
  deriving (Show,Eq,Generic)

instance ToJSON View
instance FromJSON View

emptyView :: View
emptyView = Views []
