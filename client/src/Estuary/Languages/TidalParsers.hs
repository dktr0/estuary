module Estuary.Languages.TidalParsers where

import Text.ParserCombinators.Parsec
import qualified Sound.Tidal.Context as Tidal
import Sound.Tidal.MiniTidal
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.Types.TidalParser
import Estuary.Languages.CQenze
import Estuary.Languages.Morelia
import Estuary.Languages.Saborts
import Estuary.Languages.Saludos
import Estuary.Languages.ColombiaEsPasion
import Estuary.Languages.Si
import Estuary.Languages.Sentidos
import Estuary.Languages.Natural
import Estuary.Languages.Medellin
import Estuary.Languages.LaCalle
import Estuary.Languages.Maria
import Estuary.Languages.Crudo
import Estuary.Languages.Puntoyya
import Estuary.Languages.Sucixxx
import Estuary.Languages.Vocesotrevez
import Estuary.Languages.Imagina
import Estuary.Languages.Alobestia
import Estuary.Languages.Togo
import Estuary.Languages.BlackBox
import Estuary.Languages.EspacioTiempo.Escribir


tidalParsers :: [TidalParser]
tidalParsers = [MiniTidal,CQenze,Morelia,Saborts,
  Saludos,ColombiaEsPasion,Si,Sentidos,Natural,Medellin,LaCalle,
  Maria,Crudo,Puntoyya,Sucixxx,Vocesotrevez,Imagina,Alobestia,Togo, BlackBox, Escribir2
  ]


tidalParser :: TidalParser -> Text -> Either String Tidal.ControlPattern
tidalParser MiniTidal = miniTidal . T.unpack
tidalParser CQenze = first show . cqenzeControlPattern . T.unpack
tidalParser Morelia = first show . morelia . T.unpack
tidalParser Saborts = first show . saborts . T.unpack
tidalParser Saludos = first show . saludos . T.unpack
tidalParser ColombiaEsPasion = first show . colombiaEsPasion . T.unpack
tidalParser Si = first show . si . T.unpack
tidalParser Sentidos = first show . sentidos . T.unpack
tidalParser Natural = first show . natural . T.unpack
tidalParser Medellin = first show . medellin . T.unpack
tidalParser LaCalle = first show . laCalle . T.unpack
tidalParser Maria = first show . maria . T.unpack
tidalParser Crudo = first show . crudo . T.unpack
tidalParser Puntoyya = first show . puntoyya . T.unpack
tidalParser Sucixxx = first show . sucixxx . T.unpack
tidalParser Vocesotrevez = first show . vocesotrevez . T.unpack
tidalParser Imagina = first show . imagina . T.unpack
tidalParser Alobestia = first show . alobestia . T.unpack
tidalParser Togo = first show . togo . T.unpack
tidalParser BlackBox = first show . blackBox . T.unpack
tidalParser Escribir = first show . escribir . T.unpack
