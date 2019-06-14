module Estuary.Languages.TidalParsers where

import Text.ParserCombinators.Parsec
import qualified Sound.Tidal.Context as Tidal
import Sound.Tidal.MiniTidal

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

tidalParser :: TidalParser -> String -> Either ParseError Tidal.ControlPattern
tidalParser MiniTidal = miniTidal
tidalParser CQenze = cqenzeControlPattern
tidalParser Morelia = morelia
tidalParser Saborts = saborts
tidalParser Saludos = saludos
tidalParser ColombiaEsPasion = colombiaEsPasion
tidalParser Si = si
tidalParser Sentidos = sentidos
tidalParser Natural = natural
tidalParser Medellin = medellin
tidalParser LaCalle = laCalle
tidalParser Maria = maria
tidalParser Crudo = crudo
tidalParser Puntoyya = puntoyya
tidalParser Sucixxx = sucixxx
tidalParser Vocesotrevez = vocesotrevez
tidalParser Imagina = imagina
tidalParser Alobestia = alobestia
tidalParser Togo = togo
tidalParser BlackBox = blackBox
