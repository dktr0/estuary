module Estuary.Widgets.Tutorial

import Reflex
import Reflex.Dom

import Data.Map as M

import Estuary.Types.Tutorial

tutorials::[Tutorial]
tutorials = []

tutorialWidget :: MonadWidget t m => Tutorial -> Dynamic t Context -> m (Dynamic t DefinitionMap, Event t Hint)
tutorialWidget t ctx = do
  pageMap <- forDyn ctx $ (M.lookup (pages t) . language) -- Dyn [(View,DefMap)]
  backButton <- liftM (<$ 1) $ button "back"
  nextButton <- liftM (<$ (-1)) $ button "next"
  pageNum <- foldDyn (+) 0 $ leftmost [backButton, nextButton]
  pageNumSafe <- combineDyn (\pn p-> max 0 $ min (length p) pn) pageNum pageMap -- TODO make safer
  page <- combineDyn (M.!!) pageNumSafe pageMap -- TODO Make safe
  let rebuild = fmap (\(v,dm) -> viewWidget v dm never) $ updated page
  (defMap, req, hints) <- widgetHold (viewWidget (head v) (head d) never) page
  return (defMap, hints)
