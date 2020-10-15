{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.AspectRatio where

import Data.Text
import GHC.Generics
import Data.Aeson

data AspectRatio
  = FourOverThree
  | ThreeOverFour
  | SixteenOverNine
  | NineOverSixteen
  | Square
  | Custom Rational
  deriving (Generic)

instance ToJSON AspectRatio where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON AspectRatio

instance Show AspectRatio where
  show FourOverThree = "4:3"
  show ThreeOverFour = "3:4"
  show SixteenOverNine = "16:9"
  show NineOverSixteen = "9:16"
  show Square = "1:1"
  show (Custom x) = show x
