module Estuary.Widgets.StackedPatterns where

import Reflex
import Reflex.Dom
import qualified Sound.Tidal.Context as Tidal
import Estuary.Tidal.Types
import Estuary.Reflex.Container
import Estuary.Widgets.Generic
import Estuary.Widgets.PatternChain
import Control.Monad
import Data.Map

-- from Estuary.Tidal.Types:
-- data StackedPatterns = StackedPatterns [PatternChain]

stackedPatternsWidget :: MonadWidget t m => StackedPatterns -> Event t () -> m (Dynamic t (StackedPatterns,Event t ()))
stackedPatternsWidget (StackedPatterns xs) _ = elAttr "table" ("class"=:"stackedPatternTable") $ elAttr "tr" ("class"=:"stackedPatternTable-tr") $ do
  c <- wfor xs $ \x -> elAttr "td" ("class"=:"stackedPatternTable-td") $ do
    y <- iclcForStacked x never
    mapDyn fst y
  mapDyn (\x -> (StackedPatterns x,never)) c

twoStackedPatterns :: MonadWidget t m => m (Dynamic t (StackedPatterns,Event t (), Event t Hint))
twoStackedPatterns = do
  x <- divClass "twoStackedPatternsLeft" $ iclcForStacked EmptyTransformedPattern never
  mapDyn (show . fst) x >>= dynText
  y <- divClass "twoStackedPatternsRight" $ iclcForStacked EmptyTransformedPattern never
  x' <- mapDyn fst x
  y' <- mapDyn fst y
  combineDyn (\a b -> (StackedPatterns [a,b],never,never)) x' y'


textPanel :: MonadWidget t m => m (Dynamic t (String,Event t ())) -- event is "evaluation"
textPanel = do
  eval <- button "evaluate"
  t <- textArea def
  mapDyn (\x -> (x,eval)) $ _textArea_value t


--twoPanels :: MonadWidget t m => m (Dynamic t (StackedPatterns,Event t (Either TransformedPattern String), Event t Hint))
--twoPanels = do
--  p1 <- iclcForStacked EmptyTransformedPattern never
--  p2 <- textPanel
--  v1 <- mapDyn (\(a,_,_) -> Left a) p1
--  v1' <- mapDyn (\(a,_,_) -> a) p1
--  v2 <- mapDyn (\(a,_) -> Right a ) p2
--  let u1 = updated v1
--  let u2 = updated v2
--  let updateEvents = leftmost [u1,u2]
--  h1 <- liftM (switchPromptlyDyn . fst) $ p1
--  h2 <- liftM (switchPromptlyDyn . fst) $ p2
--  hintEvents <- leftmost [h1,h2]
--  mapDyn (\x -> (StackedPatterns [x],updateEvents,hintEvents)) v1
