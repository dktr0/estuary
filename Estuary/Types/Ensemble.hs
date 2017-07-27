module Estuary.Types.Ensemble where

import qualified Data.Map.Strict as Map

import Estuary.Types.Definition
import Estuary.Types.View


data Ensemble = Ensemble {
  password :: String,
  defs :: Map.Map Int Definition,
  views :: Map.Map String View
  }

emptyEnsemble :: Ensemble
emptyEnsemble = Ensemble {
  password = "", 
  defs = Map.empty,
  views = Map.empty
  }

setPassword :: String -> Ensemble -> Ensemble
setPassword s e = e { password = s } 

editDef :: Int -> Definition -> Ensemble -> Ensemble
editDef z d s = s { defs = Map.insert z d (defs s) }

editView :: String -> View -> Ensemble -> Ensemble 
editView w v s = s { views = Map.insert w v (views s) }

