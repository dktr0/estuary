module Estuary.Languages.TidalParsers where

import Text.ParserCombinators.Parsec
import qualified Sound.Tidal.Context as Tidal
import Sound.Tidal.Parse
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.Types.TidalParser
import Estuary.Languages.CQenze
import Estuary.Languages.LaCalle
import Estuary.Languages.Sucixxx
import Estuary.Languages.Togo
import Estuary.Languages.BlackBox
import Estuary.Languages.TiempoEspacio.Escribir
import Estuary.Languages.TiempoEspacio.Observar
import Estuary.Languages.TiempoEspacio.Leer


tidalParsers :: [TidalParser]
tidalParsers = [
  MiniTidal,CQenze,LaCalle,Sucixxx,Togo, BlackBox, Escribir, Observar, Leer
  ]


tidalParser :: TidalParser -> Text -> Either String Tidal.ControlPattern
tidalParser MiniTidal = parseTidal . T.unpack
tidalParser CQenze = first show . cqenzeControlPattern . T.unpack
tidalParser LaCalle = first show . laCalle . T.unpack
tidalParser Sucixxx = first show . sucixxx . T.unpack
tidalParser Togo = first show . togo . T.unpack
tidalParser BlackBox = first show . blackBox . T.unpack
tidalParser Escribir = first show . escribir . T.unpack
tidalParser Observar = first show . observar . T.unpack
tidalParser Leer = first show . leer . T.unpack
