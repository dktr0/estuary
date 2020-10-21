{-# LANGUAGE OverloadedStrings #-}

module Estuary.Types.Settings where

import Data.Text
import qualified Sound.Punctual.Resolution as Punctual

import Estuary.Types.Language
import Estuary.Render.DynamicsMode

-- Settings are aspects of the Estuary client's behaviour that are changed only
-- by infrequent local user actions

data Settings = Settings {
  webDirtOn :: Bool,
  superDirtOn :: Bool,
  canvasOn :: Bool,
  theme :: Text,
  resolution :: Punctual.Resolution,
  brightness :: Double,
  dynamicsMode :: DynamicsMode,
  language :: Language
}

defaultSettings :: Settings
defaultSettings = Settings {
  webDirtOn = True,
  superDirtOn = False,
  canvasOn = True,
  theme = "../css-custom/classic.css",
  resolution = Punctual.HD,
  brightness = 1.0,
  dynamicsMode = DefaultDynamics,
  language = English
  }
