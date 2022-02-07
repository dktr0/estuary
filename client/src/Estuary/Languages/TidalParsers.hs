module Estuary.Languages.TidalParsers where

import Text.ParserCombinators.Parsec
import qualified Sound.Tidal.Context as Tidal
import Sound.Tidal.Parse
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.Types.TidalParser



tidalParsers :: [TidalParser]
tidalParsers = [
  MiniTidal
  ]


tidalParser :: TidalParser -> Text -> Either String Tidal.ControlPattern
tidalParser MiniTidal = parseTidal . T.unpack
