{-# LANGUAGE OverloadedStrings, RecursiveDo #-}

module Estuary.Widgets.Tutorial (runTutorial) where

import Reflex
import Reflex.Dom
import Control.Monad

import Data.Sequence as Seq
import Estuary.Types.Tutorial
import Estuary.Types.Language
import Estuary.Types.EnsembleResponse
import Estuary.Types.EnsembleRequest
import Estuary.Widgets.W
import Estuary.Widgets.View

runTutorial :: MonadWidget t m => Tutorial -> Event t [EnsembleResponse]
  -> W t m ()
runTutorial t responsesDown = divClass "tutorialContainer" $ do
  divClass "tutorialTitle code-font" $ do
    translatableText (tutorialTitle t) >>= dynText
  curPage <- divClass "rowOfButtons" $ mdo
    let nPages = Seq.length $ tutorialPages t
    cp <- holdDyn 0 pageNavEvents
    let prevPage = fmap (\x -> max (x-1) 0) cp
    let nextPage = fmap (\x -> min (x+1) (nPages-1)) cp
    navPrev <- liftM (tag $ current prevPage) $ divClass "prevNextButtons" $ do button "←" -- *** TODO: should be translated + active/inactive text, disactivated when on first page
    navNext <- liftM (tag $ current nextPage) $ divClass "prevNextButtons" $ do button "→" -- *** TODO: should be translated + active/inactive
    let pageNavEvents = leftmost [navPrev,navNext]
    return cp
  let initialPage = runTutorialPage (index (tutorialPages t) 0) responsesDown
  let builder = fmap (\x -> runTutorialPage (index (tutorialPages t) x) responsesDown) $ updated curPage
  _ <- widgetHold initialPage builder
  return ()

runTutorialPage :: MonadWidget t m => TutorialPage -> Event t [EnsembleResponse]
  -> W t m ()
runTutorialPage p responsesDown = do
  divClass "tutorialPageTitle code-font" $ do
    translatableText (tutorialPageTitle p) >>= dynText
  viewWidget responsesDown $ tutorialPageView p
