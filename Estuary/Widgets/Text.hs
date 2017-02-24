module Estuary.Widgets.Text where

import Reflex
import Reflex.Dom
import Estuary.Reflex.Container
import qualified Estuary.Widgets.GeneralPattern as G
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Data.Map
import Estuary.Widgets.Generic
import Control.Monad
import Data.List(intersperse, findIndex)
import GHCJS.DOM.EventM
import Data.Maybe(isJust)
import Text.Read(readMaybe)


textGeneralContainer :: (MonadWidget t m, Show a) => GeneralPattern a -> Event t () -> m (Dynamic t (GeneralPattern a, Event t ()))
textGeneralContainer i e = do
  let i' = show i
  x <- textInput def
  mapDyn (\x -> (TextPattern x, never)) $ _textInput_value x


textSpecificContainer :: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t ()))
textSpecificContainer (Accelerate x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Accelerate a, never))
textSpecificContainer (Bandq x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Bandq a, never))
textSpecificContainer (Begin x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Begin a, never))
textSpecificContainer (Delay x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Delay a, never))
textSpecificContainer (Delayfeedback x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Delayfeedback a, never))
textSpecificContainer (Delaytime x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Delaytime a, never))
textSpecificContainer (End x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (End a, never))
textSpecificContainer (Gain x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Gain a, never))
textSpecificContainer (Hresonance x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Hresonance a, never))
textSpecificContainer (Pan x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Pan a, never))
textSpecificContainer (Resonance x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Resonance a, never))
textSpecificContainer (Shape x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Shape a, never))
textSpecificContainer (Speed x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Speed a, never))
textSpecificContainer (Up x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Up a, never))
textSpecificContainer (Bandf x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Bandf a, never))
textSpecificContainer (Coarse x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Coarse a, never))
textSpecificContainer (Crush x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Crush a, never))
textSpecificContainer (Estuary.Tidal.Types.Cut x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Estuary.Tidal.Types.Cut a, never))
textSpecificContainer (Cutoff x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Cutoff a, never))
textSpecificContainer (Hcutoff x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Hcutoff a, never))
textSpecificContainer (Loop x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Loop a, never))
textSpecificContainer (N x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (N a, never))
textSpecificContainer (S x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (S a, never))


textPatternChain :: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t()))
textPatternChain _ _ = do
  divClass "noClass" $ text "s"
  s <- textSpecificContainer (S (TextPattern "")) never >>= toTransformedPattern
  divClass "noClass" $ text "n"
  n <- textSpecificContainer (N (TextPattern "")) never >>= toTransformedPattern
  divClass "noClass" $ text "up"
  up <- textSpecificContainer (Up (TextPattern "")) never >>= toTransformedPattern
  sn <- combineDyn (,) s n
  val <- combineDyn (\(a,b) c -> PatternChain' a Merge $ PatternChain' b Merge $ PatternChain c) sn up
  display val
  mapDyn (\x -> (x,never)) val
  where
    toTransformedPattern = mapDyn (\(x,_) -> TransformedPattern [] x)
