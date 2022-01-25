{-# LANGUAGE OverloadedStrings #-}

module Estuary.Client.Settings
  (
  Settings(..),
  getSettingsFromURI
  ) where

import GHCJS.DOM
import GHCJS.DOM.Types hiding (JSString,Text)
import GHCJS.DOM.Window as Window
import Reflex.Dom.Location
import Network.URI
import Text.Parsec
import Control.Monad.Identity
import Data.Time
import Data.Text

import qualified Sound.Punctual.Resolution as Punctual

import Estuary.Types.Language
import Estuary.Render.DynamicsMode

import Control.Monad.IO.Class (liftIO)

data Settings = Settings {

  -- settings affecting appearance/behaviour of UI
  language :: Language,
  theme :: Text,
  terminalVisible :: Bool,
  sideBarVisible :: Bool,
  statsVisible :: Bool,
  headerVisible :: Bool,

  -- settings affecting rendering of visuals
  canvasOn :: Bool,
  resolution :: Punctual.Resolution,
  brightness :: Double,
  fpsLimit :: Maybe NominalDiffTime,
  cineCer0ZIndex :: Int,
  punctualZIndex :: Int,
  improvizZIndex :: Int,
  hydraZIndex :: Int,

  -- settings affecting rendering of audio
  webDirtOn :: Bool,
  unsafeModeOn :: Bool,
  superDirtOn :: Bool,
  dynamicsMode :: DynamicsMode,
  globalAudioDelay :: Double,
  punctualAudioInputMode :: PunctualAudioInputMode

  }


defaultSettings :: Settings
defaultSettings = Settings {

  language = English,
  theme = "../css-custom/classic.css",
  terminalVisible = True,
  sideBarVisible = False,
  statsVisible = True,
  headerVisible = True,

  canvasOn = True,
  resolution = Punctual.HD,
  brightness = 1.0,
  fpsLimit = Just 0.030,
  cineCer0ZIndex = -1,
  punctualZIndex = -2,
  improvizZIndex = -3,
  hydraZIndex = -10,

  webDirtOn = True,
  unsafeModeOn = False,
  superDirtOn = False,
  dynamicsMode = DefaultDynamics,
  globalAudioDelay = 0.0,
  punctualAudioInputMode = MicToPunctual
  }

getSettingsFromURI :: MonadJSM m => m Settings
getSettingsFromURI = do
  uri <- currentWindowUnchecked >>= Window.getLocation >>= getLocationUri
  let q = uriQuery uri
  case parse settings "" q of
    Left err -> do
      liftIO $ putStrLn $ "unable to parse URL query: " ++ q
      return defaultSettings
    Right x -> return x


type P = ParsecT [Char] () Identity

type UriOption = Settings -> Settings

settings :: P Settings
settings = do
  string "?"
  justOneOption <- canvasP <|> webDirtP <|> superDirtP
  return $ justOneOption defaultSettings


canvasP :: P UriOption
canvasP = do
  string "canvas="
  x <- onOrOff
  return $ \y -> y { canvasOn = x }

webDirtP :: P UriOption
webDirtP = do
  string "webDirt="
  x <- onOrOff
  return $ \y -> y { webDirtOn = x }

superDirtP :: P UriOption
superDirtP = do
  string "superDirt="
  x <- onOrOff
  return $ \y -> y { superDirtOn = x }

unsafeModeP :: P UriOption
unsafeModeP = do
  string "unsafeMode="
  x <- onOrOff
  return $ \y -> y { unsafeModeOn = x }


onOrOff :: P Bool
onOrOff = try (string "on" >> return True) <|> (string "off" >> return False)
