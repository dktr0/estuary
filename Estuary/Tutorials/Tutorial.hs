{-# LANGUAGE RecursiveDo #-}

module Estuary.Tutorials.Tutorial where

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
import Estuary.Widgets.Generic


type TutorialPage = (View, DefinitionMap)

data TutorialId = IntroTidalText deriving (Eq, Show, Ord)

data Tutorial = Tutorial {
  tutorialId::TutorialId,
  pages:: Map Language [TutorialPage]
  }

page :: Language -> [Language -> (View,Definition)] -> TutorialPage
page lang widgets = (Views $ vs lang, M.fromList $ zip (fmap getIndex $ vs lang) (defs lang))
  where
    vs l = fst $ unzip $ fmap (\f -> f l) widgets
    defs l = snd $ unzip $ fmap (\f -> f l) widgets



generateTutorial :: [[Language->(View, Definition)]] -> Map Language [TutorialPage]
generateTutorial pgs = M.fromList $ fmap (\l -> (l,fmap (page l) pgs)) languages

-- maybe TutorialID instead? tutorialWidget :: MonadWidget t m => TutorialID -> Dynamic t Context -> m (Dynamic t DefinitionMap, Event t Hint)
tutorialWidget :: MonadWidget t m => Tutorial -> Dynamic t Context -> m (Dynamic t DefinitionMap, Event t Hint)
tutorialWidget t ctx = mdo
  currentLang <- mapDyn language ctx
  -- pageMap <- forDyn ctx $ (\c-> (M.!) (pages t) (language c)) -- Dyn [(View,DefMap)] == Dyn TutorialPage
  pageMap <- mapDyn ((M.!) (pages t)) currentLang
-- [H]   holdUniqDyn   :: Eq a => Dynamic a -> m (Dynamic a)
  -- clickableDivDynAttrs :: MonadWidget t m => String -> a -> Dynamic t (Map String String) -> m (Event t a)
  backButton <- clickableDivDynAttrs "back" () backAttrs >>= count
  nextButton <- clickableDivDynAttrs "next" () nextAttrs >>= count
  pageNum <- combineDyn (-) nextButton backButton
  pageNumSafe <- combineDyn (\pn p-> max 0 $ min (length p) pn) pageNum pageMap -- TODO make safer/fix case of over counting
  nextAttrs <- combineDyn (\n pm-> singleton "class" $ if (length pm > n+1) then "tutorial_next" else "tutorial_next displayNone") pageNumSafe pageMap
  backAttrs <- mapDyn (\n-> singleton "class" $ if (n>0) then "tutorial_back" else "tutorial_back displayNone") pageNumSafe
  page <- combineDyn (!!) pageMap pageNumSafe -- TODO Make safe
  pb <- getPostBuild
  let initialPage = attachDynWith (!!) pageMap  $ (<$) 0 pb
  let rebuild = fmap (\(v,dm) -> viewWidget v dm never) $ leftmost [updated page, initialPage]
  -- performEvent_ $ fmap (const $ lifIO $ putStrLn "test") rebuild
  debug $ fmap (const "test") initialPage
  r <- widgetHold (return (constDyn M.empty, never,never)) rebuild
  defMap <- liftM joinDyn $ mapDyn (\(a,_,_)->a) r
  hints <- liftM switchPromptlyDyn $ mapDyn (\(_,_,a)->a) r
  return (defMap, hints)
