{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Estuary.Types.WidgetOptions where

import GHC.Generics
import Data.Aeson


data CodeWidgetOptions =
  Nomenu |
  Noeval |
  Noerrors |
  Fluxus |
  Centre |
  Left |
  FontSize Float
  deriving (Show, Eq, Generic)

instance ToJSON CodeWidgetOptions where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON CodeWidgetOptions


-- and then make a Parsec parser that parses a String into such an option




-- f :: String -> Float -> CodeWidgetOptions
-- f s f =
--   | s == "nomenu" = Nomenu
--   | s == "noeval" = Noeval
--   | s == "noerrors" = Noerrors
--   | s == "fluxus" = Fluxus
--   | s == "centre" = Centre
--   | s == "left" = Left
--   | s == ("fontsize" f) = FontSize f
--
--
--   (Views [LabelView 3, CodeView 4 0 ["centre", "nomenu", "right", "fontsize: 2"] ]),
