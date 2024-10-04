{-# LANGUAGE OverloadedStrings, JavaScriptFFI #-}

module Estuary.Client.Settings
  (
  Settings(..),
  getSettingsFromURI,
  setThemeIO
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
import Data.Function ((&))

import qualified Sound.Punctual.Resolution as Punctual

import Estuary.Types.Language
import Estuary.Render.DynamicsMode

import Control.Monad.IO.Class (liftIO)

data Settings = Settings {

  -- settings affecting appearance/behaviour of UI
  noui :: Bool, -- i.e. "no UI"
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
  punctualAudioInputMode :: PunctualAudioInputMode,
  monitorInput :: Maybe Double,

  -- experimental settings (may be removed at any time!)
  minitidal :: Text
  }


defaultSettings :: Settings
defaultSettings = Settings {

  noui = False,
  language = English,
  theme = "../css-custom/classic.css",
  terminalVisible = True,
  sideBarVisible = False,
  statsVisible = True,
  headerVisible = True,

  canvasOn = True,
  resolution = Punctual.HD,
  brightness = 1.0,
  fpsLimit = Just 0.0333,
  cineCer0ZIndex = -1,
  punctualZIndex = -2,
  improvizZIndex = -3,
  hydraZIndex = -10,

  webDirtOn = True,
  unsafeModeOn = False,
  superDirtOn = False,
  dynamicsMode = DefaultDynamics,
  globalAudioDelay = 0.0,
  punctualAudioInputMode = MicToPunctual,
  monitorInput = Nothing,

  minitidal = ""
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
  xs <- sepBy uriOption (char '&')
  return $ Prelude.foldl (&) defaultSettings xs


uriOption :: P UriOption
uriOption = choice [
  try nouiP,
  try languageP,
  try themeP,
  try $ onOrOffP "canvas" (\x y -> y { canvasOn = x }),
  try $ onOrOffP "webDirt" (\x y -> y { webDirtOn = x }),
  try $ onOrOffP "superDirt" (\x y -> y { superDirtOn = x }),
  try $ onOrOffP "unsafeMode" (\x y -> y { unsafeModeOn = x }),
  try $ onOrOffP "terminal" (\x y -> y { terminalVisible = x }),
  try $ onOrOffP "sideBar" (\x y -> y { sideBarVisible = x }),
  try $ onOrOffP "stats" (\x y -> y { statsVisible = x }),
  try $ onOrOffP "header" (\x y -> y { headerVisible = x }),
  minitidalP
  ]

nouiP :: P UriOption
nouiP = string "noui" >> return (\y -> y { noui = True } )

minitidalP :: P UriOption
minitidalP = do
  string "minitidal="
  xs <- manyTill anyChar ((char '&' >> return ()) <|> lookAhead eof)
  return $ \y -> y { minitidal = pack $ unEscapeString xs }

-- note: responding to ISO 639-1 or ISO 639-2 language codes
languageP :: P UriOption
languageP = do
  string "language="
  x <- choice [
    try $ string "en" >> return English,
    try $ string "eng" >> return English,
    try $ string "es" >> return Español,
    try $ string "spa" >> return Español,
    try $ string "fr" >> return Français,
    try $ string "fre" >> return Français,
    try $ string "fra" >> return Français
    ]
  return $ \y -> y { language = x }

themeP :: P UriOption
themeP = do
  string "theme="
  t <- pathP
  return $ \y -> y { theme = pack t }

pathP :: P String
pathP = manyTill anyChar sepAhead

-- a parser that succeeds if eof or & is the next character, without consuming any characters
sepAhead :: P ()
sepAhead = (try $ lookAhead eof) <|> (try $ lookAhead $ char '&' >> return ())

onOrOffP :: String -> (Bool -> UriOption) -> P UriOption
onOrOffP pName pSetter = do
  string $ pName ++ "="
  x <- onOrOff
  return $ pSetter x

onOrOff :: P Bool
onOrOff = try (string "on" >> return True) <|> (string "off" >> return False)

foreign import javascript safe
  "document.getElementById('estuary-current-theme').setAttribute('href', $1);"
  setThemeIO :: Text -> IO ()
