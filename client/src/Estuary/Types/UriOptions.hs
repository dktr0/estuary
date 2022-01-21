module Estuary.Types.UriOptions
  (
  UriOptions(..),
  getUriOptions
  ) where

import GHCJS.DOM
import GHCJS.DOM.Types hiding (JSString)
import GHCJS.DOM.Window as Window
import Reflex.Dom.Location
import Network.URI
import Text.Parsec
import Control.Monad.Identity
import Control.Monad.IO.Class (liftIO)

data UriOptions = UriOptions {
  canvas :: Bool
  } deriving (Show)

defaultUriOptions :: UriOptions
defaultUriOptions = UriOptions {
  canvas = True
  }

getUriOptions :: MonadJSM m => m UriOptions
getUriOptions = do
  uri <- currentWindowUnchecked >>= Window.getLocation >>= getLocationUri
  let q = uriQuery uri
  case parse uriOptions "uriOptions" q of
    Left err -> do
      liftIO $ putStrLn $ "unable to parse URL query: " ++ q
      return defaultUriOptions
    Right x -> return x

type P = ParsecT [Char] () Identity

type UriOption = UriOptions -> UriOptions

uriOptions :: P UriOptions
uriOptions = do
  string "?"
  justOneOption <- canvasP
  return $ justOneOption defaultUriOptions

canvasP :: P UriOption
canvasP = do
  string "canvas="
  x <- onOrOff
  return $ \y -> y { canvas = x }

onOrOff :: P Bool
onOrOff = try (string "on" >> return True) <|> (string "off" >> return False)
