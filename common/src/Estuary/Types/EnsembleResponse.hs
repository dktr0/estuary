{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.EnsembleResponse where

import Data.Maybe (mapMaybe)
import Data.Ratio
import Text.JSON
import Text.JSON.Generic

import Estuary.Types.Sited
import Estuary.Types.EditOrEval
import Estuary.Types.View
import Estuary.Types.Tempo
import Estuary.Types.Definition

data EnsembleResponse =
  Chat String String | -- name message
  ZoneResponse (Sited Int (EditOrEval Definition)) |
  ViewList [String] |
  View (Sited String View) |
  DefaultView View |
  NewTempo Tempo |
  EnsembleClientCount Int
  deriving (Data,Typeable)

instance JSON EnsembleResponse where
  showJSON = toJSON
  readJSON = fromJSON

justEditsInZone :: Int -> [EnsembleResponse] -> [Definition]
justEditsInZone z1 = mapMaybe f
  where
    f (ZoneResponse (Sited z2 (Edit a))) | z1 ==z2 = Just a
    f _ = Nothing

justChats :: [EnsembleResponse] -> [(String,String)]
justChats = mapMaybe f
  where f (Chat x y) = Just (x,y)
        f _ = Nothing

justViews :: [EnsembleResponse] -> [Sited String View]
justViews = mapMaybe f
  where f (View x) = Just x
        f _ = Nothing
