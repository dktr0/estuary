module Estuary.Tutorials.Tutorial

import Reflex
import Reflex.Dom

import Estuary.Types.View
import Estuary.Types.Definition

import Data.Map as M

type TutorialPage = (View, DefinitionMap)

data TutorialId = IntroTidalText (Eq, Show)

data Tutorial i p = Tutorial {
  tutorialId::TutorialId,
  pages:: Map Language [TutorialPage],
}

-- To be populated once tutorials exist
tutorials::[Tutorial]
tutorials = []

page :: Language -> [(View,Definition)] -> TutorialPage
page l widgets = (Views $ vs l, M.fromList (fmap getIndex $ vs l) $ vs l)
  where
    (vs,defs) l = unzip $ fmap (\f -> f l) widgets
    
generateTutorial :: [[Language->(View, Definition)]] -> Map Language [TutorialPage]
generateTutorial pgs = M.fomList $ fmap (\l -> (l,fmap (page l) pgs)) languages

-- maybe TutorialID instead? tutorialWidget :: MonadWidget t m => TutorialID -> Dynamic t Context -> m (Dynamic t DefinitionMap, Event t Hint)
tutorialWidget :: MonadWidget t m => Tutorial -> Dynamic t Context -> m (Dynamic t DefinitionMap, Event t Hint)
tutorialWidget t ctx = do
  pageMap <- forDyn ctx $ (M.lookup (pages t) . language) -- Dyn [(View,DefMap)]
  backButton <- liftM (<$ 1) $ button "back"
  nextButton <- liftM (<$ (-1)) $ button "next"
  pageNum <- foldDyn (+) 0 $ leftmost [backButton, nextButton]
  pageNumSafe <- combineDyn (\pn p-> max 0 $ min (length p) pn) pageNum pageMap -- TODO make safer/fix case of over counting
  page <- combineDyn (M.!!) pageNumSafe pageMap -- TODO Make safe
  let rebuild = fmap (\(v,dm) -> viewWidget v dm never) $ updated page
  (defMap, req, hints) <- widgetHold (viewWidget (head v) (head d) never) page
  return (defMap, hints)
