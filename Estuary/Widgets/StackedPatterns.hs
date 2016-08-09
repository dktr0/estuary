module Estuary.Widgets.StackedPatterns where

import Reflex
import Reflex.Dom
import qualified Sound.Tidal.Context as Tidal
import Estuary.Tidal.Types
import Estuary.Reflex.Container
import Estuary.Widgets.Generic
import Control.Monad

-- wfor :: (MonadWidget t m) => [a] -> (a -> m (Dynamic t b)) -> m (Dynamic t [b])

stackedPatternsWidget :: MonadWidget t m => Int -> StackedPatterns -> Event t () -> m (Dynamic t (StackedPatterns,Event t GenericSignal))
stackedPatternsWidget columns (StackedPatterns iValue) _ = el "table" $ el "tr" $ do
  c <- wfor [1..columns] $ \_ -> el "td" $ do
    x <- trivialPatternChain placeHolder never
    mapDyn fst x
  mapDyn (\x -> (StackedPatterns x,never)) c
  where placeHolder = PatternChain (TransformedPattern [] (S (Atom "bd" Once)))


trivialPatternChain :: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
trivialPatternChain iValue _ = do
  text "trivialPatternChain"
  return $ constDyn (iValue,never)
