{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Estuary.Types.View where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Data.Aeson
import Data.Map

import Estuary.Types.TranslatableText
import Estuary.Types.TextNotation
import Estuary.Types.Language

data View =
  EmptyView |
  Div Text [View] | -- a div with an arbitrary CSS class (specified by first argument to constructor)
  Views [View] |
  Paragraph [View] | -- a block of explanatory text
  BorderDiv [View] |
  Link Text [View] | -- a clickable link
  BulletPoints [View] | -- an HTML <ul> element containing <li> elements for each child view
  GridView Int Int [View] | -- columns rows [children]
  Text TranslatableText |
  LabelView Int |
  StructureView Int |
  CodeView Int Int| -- first int is zone to edit, second int is number of lines in editor
  SequenceView Int |
  Example TextNotation Text | -- a clickable text-code example
  EnsembleStatusView |
  TempoView |
  RouletteView Int Int |
  AudioMapView |
  CountDownView Int |
  SandClockView Int |
  StopWatchView Int |
  SeeTimeView Int |
  NotePadView Int
  deriving (Show,Eq,Generic)

instance ToJSON View where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON View

link :: Text -> View
link url = Link url [Text $ fromList [(English,url)]]
