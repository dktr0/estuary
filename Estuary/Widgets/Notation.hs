module Estuary.Widgets.Notation where

import Reflex
import Reflex.Dom
import Estuary.Types.Notation
import Estuary.Widgets.Generic
import Estuary.Widgets.Container


-- notationWidget: given a basic Estuary widget function for a given main type,
-- produce a variant in which the given type is wrapped in a Notation

notationWidget :: MonadWidget t m => (a -> Event t() -> m (Dynamic t (a,Event t (EditSignal a)))) -> Notation a -> Event t () -> m (Dynamic t (Notation a,Event t (EditSignal a)))
notationWidget widget iValue events = rdo
  e <- clickableDiv' "eval" $ eval
  a <- clickableDiv' "abandon" $ abandon
  l3 <- clickableDiv' "l3" $ l3
  l4 <- clickableDiv' "l4" $ l4
  w <- resettableWidget widget (future iValue) events reset
  newValues <- fmap (fst) w >>= liftM (edit . updated)
  notations <- foldDyn ($) iValue $ leftMost [e,a,l3,l4,newValues]
  let reset = attachPromptlyDynWith (\x _ -> present x) notations a -- or does it need to be not promptly???
  fmap (\a ->(a,never)) pattern
