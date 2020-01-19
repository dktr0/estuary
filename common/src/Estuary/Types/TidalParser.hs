{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.TidalParser where

import GHC.Generics
import Data.Aeson

data TidalParser = MiniTidal | CQenze | Morelia | Saborts |
  Saludos | ColombiaEsPasion | Si | Sentidos | Natural | Medellin | LaCalle |
  Maria | Crudo | Puntoyya | Sucixxx | Vocesotrevez | Imagina | Alobestia | Togo | BlackBox -- | Escribir | Observar | Leer
  deriving (Show,Read,Eq,Ord,Generic)

instance ToJSON TidalParser where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TidalParser
