module Estuary.Types.Space where

import qualified Data.Map.Strict as Map

import Estuary.Types.Definition
import Estuary.Types.View


data Space = Space {
  defs :: Map.Map Int Definition,
  views :: Map.Map String View
  }

emptySpace :: Space
emptySpace = Space {
  defs = Map.empty,
  views = Map.empty
  }

editDef :: Int -> Definition -> Space -> Space
editDef z d s = s { defs = Map.insert z d (defs s) }
