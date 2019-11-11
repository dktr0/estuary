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
  ViewDiv Text View | -- a div with an arbitrary CSS class (specified by first argument to constructor)
  GridView Int Int [View] |
  BorderDiv View |
  LabelView Int |
  StructureView Int |
  TextView Int Int | -- first int is zone to edit, second int is number of lines in editor
  SequenceView Int |
  Paragraph TranslatableText | -- a block of explanatory text in multiple languages
  Example TextNotation Text | -- a clickable text-code example
  EnsembleStatusView
  deriving (Show,Eq,Generic)

instance ToJSON View where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON View
