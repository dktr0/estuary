{-# LANGUAGE RecursiveDo #-}

module Estuary.Tutorials.Tutorial where

import Reflex
import Reflex.Dom
import Control.Monad (liftM)
import Data.Map as M
import Data.Maybe (isNothing)

import Estuary.Types.View
import Estuary.Types.Definition
import Estuary.Types.Language
import Estuary.Types.Request
import Estuary.Types.Hint
import Estuary.Types.Context
import Estuary.Widgets.View
import Estuary.Widgets.Generic

-- data Definition =
--   Structure TransformedPattern |
--   EvaluableText String |
--   LabelText String
--   deriving (Eq,Show)
--
-- type DefinitionMap = Map.Map Int Definition

type TutorialPage = (View, DefinitionMap)

data TutorialId = IntroTidalText deriving (Eq, Show, Ord)

data Tutorial = Tutorial {
  tutorialId::TutorialId,
  pages:: Map Language [TutorialPage],
  defaultLang::Language -- shown by default when a translation DNE to current lang
  }

-- Default based on lanuage
defaultPages:: Tutorial -> [TutorialPage]
defaultPages t = maybe (maybe lastResort id $ fstElement (toList $ pages t)) id $ M.lookup (defaultLang t) (pages t)
  where
    fstElement ((l,ps):xs) = Just ps
    fstElement _ = Nothing
    lastResort = [errorView]
    errorView = (LabelView 1, singleton 1 (LabelText "Oops, an error has occurred - please raise an issue at github.com/d0kt0r0/estuary"))

toTutorialPage::[(Int->View,Definition)] -> TutorialPage
toTutorialPage xs = (Views vs, fromList defs) where
  (vs, defs) = unzip $ zipWith (\i (vf,d)-> (vf i, (i,d))) [(0::Int)..] xs  -- ([view],[defmap])

page :: Language -> [Language -> (View,Definition)] -> TutorialPage
page lang widgets = (Views $ vs lang, M.fromList $ zip (fmap getIndex $ vs lang) (defs lang))
  where
    vs l = fst $ unzip $ fmap (\f -> f l) widgets
    defs l = snd $ unzip $ fmap (\f -> f l) widgets

generateTutorial :: [[Language->(View, Definition)]] -> Map Language [TutorialPage]
generateTutorial pgs = M.fromList $ fmap (\l -> (l,fmap (page l) pgs)) languages

tutorialWidget :: MonadWidget t m => Tutorial -> Dynamic t Context -> m (Dynamic t DefinitionMap, Event t Hint)
tutorialWidget t ctx = mdo
  lang <- liftM nubDyn $ mapDyn language ctx
  pageMapMaybe <- mapDyn ((flip M.lookup) (pages t)) lang
  pageMap <- mapDyn (maybe (defaultPages t) id) pageMapMaybe
  pb <- getPostBuild
  debug $ (<$) (pages t) pb
  let iLang = attachWith (\l _ -> M.lookup l (pages t)) (fmap language $ current ctx) pb
  translationDNEWidget "translationDNEWidget" False lang $ fmap isNothing $ leftmost [updated pageMapMaybe, iLang]
  backButton <- clickableDivDynAttrs "back" () backAttrs >>= count
  nextButton <- clickableDivDynAttrs "next" () nextAttrs >>= count
  pageNum <- combineDyn (-) nextButton backButton
  pageNumSafe <- combineDyn (\pn p-> max 0 $ min (length p) pn) pageNum pageMap -- TODO make safer/fix case of over counting
  nextAttrs <- combineDyn (\n pm-> singleton "class" $ if (length pm > n+1) then "tutorial_next" else "tutorial_next displayNone") pageNumSafe pageMap
  backAttrs <- mapDyn (\n-> singleton "class" $ if (n>0) then "tutorial_back" else "tutorial_back displayNone") pageNumSafe
  page <- combineDyn (!!) pageMap pageNumSafe -- TODO Make safe
  let initialPage = attachDynWith (!!) pageMap  $ (<$) 0 pb
  let rebuild = fmap (\(v,dm) -> viewWidget v dm never) $ leftmost [updated page, initialPage]
  r <- widgetHold (return (constDyn M.empty, never,never)) rebuild
  defMap <- liftM joinDyn $ mapDyn (\(a,_,_)->a) r
  hints <- liftM switchPromptlyDyn $ mapDyn (\(_,_,a)->a) r
  return (defMap, hints)

-- translation does not exist widget
translationDNEWidget :: MonadWidget t m => String -> Bool -> Dynamic t Language -> Event t Bool -> m ()
translationDNEWidget cssClass iShowing lang b  = do
  str <- mapDyn translationDNE lang
  attrs <- holdDyn iShowing b >>= mapDyn (\x-> union (singleton "class" cssClass) (if x then empty else singleton "style" "display:none"))
  elDynAttr "div" attrs $ dynText str
  return ()







  --
