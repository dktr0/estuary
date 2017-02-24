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
  let textAttrs = constDyn $ singleton "class" "textInputToEndOfLine"
  x <- textInput $ def & textInputConfig_attributes .~ textAttrs
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
textSpecificContainer (Vowel x) e = textGeneralContainer x e >>= mapDyn (\(a,_) -> (Vowel a, never))


textPatternChain :: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t()))
textPatternChain i e = divClass "textPatternChain" $ do
  s <- divClass "labelAndTextPattern" $ do
    text "s "
    textSpecificContainer (S (TextPattern "")) never >>= toTransformedPattern
  n <- divClass "labelAndTextPattern" $ do
    text "n "
    textSpecificContainer (N (TextPattern "")) never >>= toTransformedPattern
  up <- divClass "labelAndTextPattern" $ do
    text "up "
    textSpecificContainer (Up (TextPattern "")) never >>= toTransformedPattern
  vowel <- divClass "labelAndTextPattern" $ do
    text "vowel "
    textSpecificContainer (Vowel (TextPattern "")) never >>= toTransformedPattern
  sn <- combineDyn (,) s n
  upvowel <- combineDyn (,) up vowel
  val <- combineDyn (\(a,b) (c,d) -> PatternChain' a Merge $ PatternChain' b Merge $ PatternChain' c Merge $ PatternChain d) sn upvowel
  mapDyn (\x -> (x,never)) val
  where
    toTransformedPattern = mapDyn (\(x,_) -> TransformedPattern [] x)

textInterface :: MonadWidget t m => PatternChain -> Event t () -> m (Dynamic t (PatternChain, Event t ()))
textInterface i e = do
  x <- divClass "twoStackedPatternsLeft" $ textPatternChain i e
  y <- divClass "twoStackedPatternsRight" $ divClass "paddedText" $ do
    el "div" $ text "In this interface you can enter patterns directly using the text pattern notation of the TidalCycles language. For example in the space for an 's' pattern you could enter 'bd cp' (without the quotes) to hear a bass drum and a clap. Some other possible sample names to use in 's' patterns include: sid hh arpy sine glitch tabla bass (and many more). In the space for 'n' patterns you should use whole numbers (i.e. 0 1 2 3 etc) to access different samples from the set you indicate with your s patterns. In the space for 'up' patterns you can put numbers that shift the pitch of the samples. Try an s pattern like this 'arpy*4' with an up pattern like this '0 4 7 12', for example. In the space for 'vowel' patterns you can put the vowels: a e i o u. (Note to any TidalCycles experts: the ? operator doesn't work in this context, yet!)"
  return x
