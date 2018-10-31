{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.Ensemble where

import qualified Data.Map.Strict as Map

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar
import Text.JSON
import Text.JSON.Generic
import Text.Read
import Data.Ratio

import Estuary.Types.Definition
import Estuary.Types.View
import Estuary.Types.Tempo

data Ensemble = Ensemble {
  password :: String,
  defs :: Map.Map Int Definition,
  views :: Map.Map String View,
  defaultView :: View,
  tempo :: Tempo
  } deriving (Data,Typeable)

instance JSON Ensemble where
  showJSON = toJSON
  readJSON = fromJSON

emptyEnsemble :: UTCTime -> Ensemble
emptyEnsemble t = Ensemble {
  password = "",
  defs = Map.empty,
  views = Map.empty,
  defaultView = standardView,
  tempo = Tempo { at=t, beat=0.0, cps=0.5 }
  }

setPassword :: String -> Ensemble -> Ensemble
setPassword s e = e { password = s }

editDef :: Int -> Definition -> Ensemble -> Ensemble
editDef z d s = s { defs = Map.insert z d (defs s) }

editDefaultView :: View -> Ensemble -> Ensemble
editDefaultView v s = s { defaultView = v }

editView :: String -> View -> Ensemble -> Ensemble
editView w v s = s { views = Map.insert w v (views s) }

deleteView :: String -> Ensemble -> Ensemble
deleteView v e = e { views = Map.delete v (views e) }

tempoChange :: Tempo -> Ensemble -> Ensemble
tempoChange t e = e { tempo = t }
