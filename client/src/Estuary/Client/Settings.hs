module Estuary.Client.Settings
  (
  Settings(..),
  getSettingsFromURI
  ) where

import GHCJS.DOM
import GHCJS.DOM.Types hiding (JSString)
import GHCJS.DOM.Window as Window
import Reflex.Dom.Location
import Network.URI
import Text.Parsec
import Control.Monad.Identity
import Control.Monad.IO.Class (liftIO)

data Settings = Settings {
  canvasOn :: Bool,
  webDirtOn :: Bool,
  superDirtOn :: Bool,
  unsafeModeOn :: Bool
  } deriving (Show)

defaultSettings :: Settings
defaultSettings = Settings {
  canvasOn = True,
  webDirtOn = True,
  superDirtOn = False,
  unsafeModeOn = False
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
