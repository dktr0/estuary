module Estuary.Languages.TidalParsers where

import Text.ParserCombinators.Parsec
import qualified Sound.Tidal.Context as Tidal

import Estuary.Types.TidalParser
import Estuary.Languages.MiniTidal
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


tidalParsers :: [TidalParser]
tidalParsers = [MiniTidal,CQenze,Morelia,Saborts,
  Saludos,ColombiaEsPasion,Si,Sentidos,Natural,Medellin,LaCalle,
  Maria,Crudo,Puntoyya,Sucixxx,Vocesotrevez,Imagina,Alobestia
  ]

tidalParser :: TidalParser -> String -> Either ParseError Tidal.ParamPattern
tidalParser MiniTidal = miniTidalParser
tidalParser CQenze = cqenzeParamPattern
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
