module Estuary.Tutorials.Tutorial (generateTutorial, tutorials) where

import Reflex
import Reflex.Dom
import Control.Monad (liftM)
import Data.Map as M

import Estuary.Types.View
import Estuary.Types.Definition
import Estuary.Types.Language
import Estuary.Types.Request
import Estuary.Types.Hint
import Estuary.Types.Context
import Estuary.Widgets.View

import Estuary.Tutorial.IntroTidalText


type TutorialPage = (View, DefinitionMap)

data TutorialId = IntroTidalText deriving (Eq, Show, Ord)

data Tutorial = Tutorial {
  tutorialId::TutorialId,
  pages:: Map Language [TutorialPage]
  }

-- To be populated once tutorials exist
tutorials:: Map TutorialID Tutorial
tutorials = M.fromList $ fmap (\x->(tutorialId x,x)) [introTidalText]

page :: Language -> [Language -> (View,Definition)] -> TutorialPage
page lang widgets = (Views $ vs lang, M.fromList $ zip (fmap getIndex $ vs lang) (defs lang))
  where
    vs l = fst $ unzip $ fmap (\f -> f l) widgets
    defs l = snd $ unzip $ fmap (\f -> f l) widgets



generateTutorial :: [[Language->(View, Definition)]] -> Map Language [TutorialPage]
generateTutorial pgs = M.fromList $ fmap (\l -> (l,fmap (page l) pgs)) languages

-- maybe TutorialID instead? tutorialWidget :: MonadWidget t m => TutorialID -> Dynamic t Context -> m (Dynamic t DefinitionMap, Event t Hint)
tutorialWidget :: MonadWidget t m => Tutorial -> Dynamic t Context -> m (Dynamic t DefinitionMap, Event t Hint)
tutorialWidget t ctx = do
  pageMap <- forDyn ctx $ (\c-> (M.!) (pages t) (language c)) -- Dyn [(View,DefMap)] == Dyn TutorialPage
  backButton <- button "back" >>= count
  nextButton <- button "next" >>= count
  pageNum <- combineDyn (-) nextButton backButton
  pageNumSafe <- combineDyn (\pn p-> max 0 $ min (length p) pn) pageNum pageMap -- TODO make safer/fix case of over counting
  page <- combineDyn (!!) pageMap pageNumSafe -- TODO Make safe

  pb <- getPostBuild
  let initialPage = attachDynWith (!!) pageMap  $ (<$) 0 pb
  let rebuild = fmap (\(v,dm) -> viewWidget v dm never) $ leftmost [updated page, initialPage]

  r <- widgetHold (return (constDyn M.empty, never,never)) rebuild
  defMap <- liftM joinDyn $ mapDyn (\(a,_,_)->a) r
  hints <- liftM switchPromptlyDyn $ mapDyn (\(_,_,a)->a) r

  -- dynWidget <- forDyn page $ (\(v,dm) -> viewWidget v dm never)
  -- defMap <- liftM joinDyn $ mapDyn (\(d,_,_)-> d) dynWidget
  -- hs <- liftM switchPromptlyDyn $ mapDyn (\(_,_,h) -> h) dynWidget
  return (defMap, hints)

  --
  -- Dynamic t (Dynamic t DefinitionMap, t1, (Dynamic t0 DefinitionMap, Event t0 (Estuary.Types.EnsembleRequest.EnsembleRequest Definition),
  --                       Event t0 Hint))
  --
  -- viewWidget :: MonadWidget t m => View -> DefinitionMap -> Event t [EnsembleResponse Definition] ->
  --   m (Dynamic t DefinitionMap, Event t (EnsembleRequest Definition), Event t Hint)
