module Estuary.Languages.TidalParsers where

import Text.ParserCombinators.Parsec
import qualified Sound.Tidal.Context as Tidal
import Sound.Tidal.Parse
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T

tidalParser :: Text -> Either String Tidal.ControlPattern
tidalParser = parseTidal . T.unpack
