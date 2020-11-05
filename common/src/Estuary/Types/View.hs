{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Estuary.Types.View where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Data.Aeson

import Estuary.Types.TranslatableText
import Estuary.Types.TextNotation

data View =
  EmptyView |
  Views [View] |
  Div Text View | -- a div with an arbitrary CSS class (specified by first argument to constructor)
  GridView Int Int [View] | -- columns rows [children]
  BorderDiv View |
  LabelView Int |
  StructureView Int |
  TextView Int Int| -- first int is zone to edit, second int is number of lines in editor
  SequenceView Int |
  Paragraph View | -- a block of explanatory text
  TranslatableText TranslatableText
  Link Text TranslatableText | -- a clickable link
  Example TextNotation Text | -- a clickable text-code example
  EnsembleStatusView |
  TempoView |
  RouletteView Int Int Bool |
  AudioMapView |
  StopWatchView Int
  deriving (Show,Eq,Generic)

instance ToJSON View where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON View
