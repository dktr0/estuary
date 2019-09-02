{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Estuary.Types.View where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson

data View =
  Views [View] |
  ViewDiv Text View | -- deprecated in favour of next two...
  RowView Rational View | -- a full row in the layout, number is percent of vertical space
  CellView Rational View | -- a cell (ie. column fragment) in the layout, number is percent of horizontal space
  LabelView Int |
  StructureView Int |
  TextView Int Int | -- first int is zone to edit, second int is number of lines in editor
  SequenceView Int
  deriving (Show,Eq,Generic)

instance ToJSON View where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON View

emptyView :: View
emptyView = Views []

viewToRows :: View -> Int
viewToRows (Views vs) = sum $ fmap viewToRows vs
viewToRows (ViewDiv _ v) = viewToRows v
viewToRows (RowView _ v) = viewToRows v
viewToRows (CellView _ v) = viewToRows v
viewToRows (LabelView _) = 1
viewToRows (StructureView _) = 1 -- ???
viewToRows (SequenceView _) = 1 -- ???
viewToRows (TextView _ n) = n + 1
