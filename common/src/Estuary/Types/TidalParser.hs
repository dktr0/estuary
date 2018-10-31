{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.TidalParser where

import Text.JSON
import Text.JSON.Generic

data TidalParser = MiniTidal | CQenze | Morelia | Saborts |
  Saludos | ColombiaEsPasion | Si | Sentidos | Natural | Medellin | LaCalle |
  Maria | Crudo | Puntoyya | Sucixxx | Vocesotrevez | Imagina | Alobestia
  deriving (Show,Read,Eq,Ord,Data,Typeable)

instance JSON TidalParser where
  showJSON = toJSON
  readJSON = fromJSON
