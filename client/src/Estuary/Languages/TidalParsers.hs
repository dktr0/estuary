module Estuary.Languages.TidalParsers where

import Text.ParserCombinators.Parsec
import qualified Sound.Tidal.Context as Tidal
import Sound.Tidal.MiniTidal
import Data.Bifunctor (first)

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


tidalParsers :: [TidalParser]
tidalParsers = [MiniTidal,CQenze,Morelia,Saborts,
  Saludos,ColombiaEsPasion,Si,Sentidos,Natural,Medellin,LaCalle,
  Maria,Crudo,Puntoyya,Sucixxx,Vocesotrevez,Imagina,Alobestia,Togo, BlackBox
  ]


tidalParser :: TidalParser -> String -> Either String Tidal.ControlPattern
tidalParser MiniTidal = miniTidal
tidalParser CQenze = first show . cqenzeControlPattern
tidalParser Morelia = first show . morelia
tidalParser Saborts = first show . saborts
tidalParser Saludos = first show . saludos
tidalParser ColombiaEsPasion = first show . colombiaEsPasion
tidalParser Si = first show . si
tidalParser Sentidos = first show . sentidos
tidalParser Natural = first show . natural
tidalParser Medellin = first show . medellin
tidalParser LaCalle = first show . laCalle
tidalParser Maria = first show . maria
tidalParser Crudo = first show . crudo
tidalParser Puntoyya = first show . puntoyya
tidalParser Sucixxx = first show . sucixxx
tidalParser Vocesotrevez = first show . vocesotrevez
tidalParser Imagina = first show . imagina
tidalParser Alobestia = first show . alobestia
tidalParser Togo = first show . togo
tidalParser BlackBox = first show . blackBox
