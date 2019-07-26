{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Estuary.Types.View where

import Text.JSON
import Text.JSON.Generic
import Data.Text (Text)

data View =
  Views [View] |
  ViewDiv Text View |
  LabelView Int |
  StructureView Int |
  TextView Int Int | -- first int is zone to edit, second int is number of lines in editor
  SequenceView Int
  deriving (Show,Eq,Data,Typeable)

instance JSON View where
  showJSON = toJSON
  readJSON = fromJSON

emptyView :: View
emptyView = Views []
