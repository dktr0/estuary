module Estuary.Widgets.PatternTransformer where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Data.Map

trivialPatternTransformer :: MonadWidget t m => m (Dynamic t (PatternTransformer,()))
trivialPatternTransformer = el "div" $ do
  let ddItems = [NoTransformer,Rev,Slow 2,Density 2,Degrade,DegradeBy 0.9,Brak]
  let ddShow = Prelude.map (show) ddItems
  let ddMap = constDyn $ fromList $ zip [0::Int,1,2,3,4,5,6] ddShow
  dd <- dropdown 0 ddMap def
  dd' <- mapDyn (ddItems!!) $ _dropdown_value dd
  dd'' <- mapDyn (ddShow!!) $ _dropdown_value dd
  display dd''
  mapDyn (\a -> (a,())) $ dd'
