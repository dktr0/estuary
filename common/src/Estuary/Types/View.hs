{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Estuary.Types.View where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Data.Aeson


data View =
  Views [View] |
  GridView Int Int [View] |
  ViewDiv Text View |
  BorderDiv View |
  RowView Rational View | -- a full row in the layout, number is percent of vertical space
  CellView Rational View | -- a cell (ie. column fragment) in the layout, number is percent of horizontal space
  LabelView Int |
  StructureView Int |
  TextView Int Int| -- first int is zone to edit, second int is number of lines in editor
  SequenceView Int |
  EnsembleStatusView
  deriving (Show,Eq,Generic)

instance ToJSON View where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON View

emptyView :: View
emptyView =  Views []
