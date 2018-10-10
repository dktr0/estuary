{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.TextNotation where

import Text.JSON
import Text.JSON.Generic
import qualified Sound.Tidal.Context as Tidal

import Estuary.Languages.TidalParser

data TextNotation =
  TidalTextNotation TidalParser
  deriving (Show,Read,Eq,Ord,Data,Typeable)

instance JSON TextNotation where
  showJSON = toJSON
  readJSON = fromJSON

-- *** note: the definition below should evolve soon to return Maybe Tidal.ParamPattern
tidalTextToParamPattern :: (TextNotation,String) -> Tidal.ParamPattern
tidalTextToParamPattern (TidalTextNotation x,y) = tidalParser x y
tidalTextToParamPattern _ = Tidal.silence
