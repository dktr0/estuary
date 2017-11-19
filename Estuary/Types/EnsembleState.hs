module Estuary.Types.EnsembleState where

import Data.Map

import Estuary.Types.View

data EnsembleState = EnsembleState {
  ensembleName :: String,
  handle :: String,
  publishedViews :: Map String View,
  activeView :: View,
  activeViewName :: String
}
