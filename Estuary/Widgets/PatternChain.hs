module Estuary.Widgets.PatternChain where

import Reflex
import Reflex.Dom
import qualified Sound.Tidal.Context as Tidal
import Estuary.Tidal.Types
import Estuary.Reflex.Container
import Estuary.Widgets.Generic
import Estuary.Reflex.Utility
import Control.Monad
import Data.Map

-- from Estuary.Tidal.Types:
-- data PatternCombinator = Merge | Add | Subtract | Multiply | Divide deriving (Eq,Show)
-- data PatternChain = PatternChain TransformedPattern | PatternChain' TransformedPattern PatternCombinator PatternChain deriving (Eq)


trivialPatternChain :: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
trivialPatternChain iValue _ = do
  x <- button' "just an S atom" $ PatternChain (TransformedPattern [] (S (Atom "bd" Once)))
  el "br" blank
  y <- button' "merge S and N" $ PatternChain' (TransformedPattern [] (S (Atom "hh" (Rep 4)))) Merge (PatternChain (TransformedPattern [] (nPatternFromList [0..3])))
  el "br" blank
  z <- button' "adding Ns" $ PatternChain' (TransformedPattern [] (N (Atom 60 Once))) Add (PatternChain (TransformedPattern [] (N (Atom 7 Once))))
  el "br" blank
  xyz <- holdDyn iValue $ leftmost [x,y,z]
  mapDyn (\a -> (a,never)) xyz


{-
patternChainWidget :: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t GenericSignal))
patternChainWidget (PatternChain tp) _ = do
  tpWidget <- trivialTransformedPattern

patternChainWidget (PatternChain' tp combinator next) _ = do
-}
