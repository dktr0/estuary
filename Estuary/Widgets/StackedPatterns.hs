module Estuary.Widgets.StackedPatterns where

import Reflex
import Reflex.Dom
import qualified Sound.Tidal.Context as Tidal
import Estuary.Tidal.Types
import Estuary.Widgets.Generic
import Control.Monad


stackedPatternsWidget :: MonadWidget t m => Int -> Int -> StackedPatterns -> Event t () -> m (Dynamic t (StackedPatterns,Event t GenericSignal))
stackedPatternsWidget rows columns (StackedPatterns iValue) _ = el "table"  $ do
  r <- forM [1..rows] $ \r -> el "tr" $ do
    c <- forM [1..columns] $ \c -> el "td" $ trivialPatternChain placeHolder never
    holdDyn 
  let flat = concat rows -- m [Dynamic t (PatternChain,Event)]
  firsts <- mapM (mapDyn (fst)) flat -- m [Dynamic t PatternChain]
  dynList <- collectDyn firsts
  mapDyn (\x -> (StackedPatterns x,never)) dynList
  where placeHolder = PatternChain (TransformedPattern [] (S (Atom "bd" Once)))


trivialPatternChain :: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
trivialPatternChain iValue _ = do
  text "trivialPatternChain"
  return $ constDyn (iValue,never)
