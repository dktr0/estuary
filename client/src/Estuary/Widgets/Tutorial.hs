{-# LANGUAGE OverloadedStrings, RecursiveDo #-}

module Estuary.Widgets.Tutorial (runTutorial) where

import Reflex
import Reflex.Dom
import Control.Monad

import Data.Sequence as Seq
import Estuary.Types.Tutorial
import Estuary.Types.EnsembleResponse
import Estuary.Types.EnsembleRequest
import Estuary.Widgets.Editor
import Estuary.Widgets.View

runTutorial :: MonadWidget t m => Tutorial -> Event t [EnsembleResponse]
  -> Editor t m (Event t EnsembleRequest)
runTutorial t responsesDown = do
  translatedText $ tutorialTitle t
  curPage <- liftR $ mdo
    let nPages = Seq.length $ tutorialPages t
    cp <- holdDyn 0 pageNavEvents
    let prevPage = fmap (\x -> max (x-1) 0) cp
    let nextPage = fmap (\x -> min (x+1) (nPages-1)) cp
    navPrev <- liftM (tag $ current prevPage) $ button "prev" -- *** TODO: should be translated + active/inactive text, disactivated when on first page
    navNext <- liftM (tag $ current nextPage) $ button "next" -- *** TODO: should be translated + active/inactive
    let pageNavEvents = leftmost [navPrev,navNext]
    return cp
  let initialPage = runTutorialPage (index (tutorialPages t) 0) responsesDown
  let builder = fmap (\x -> runTutorialPage (index (tutorialPages t) x) responsesDown) $ updated curPage
  thePage <- editorHold initialPage builder
  return $ switchDyn thePage

runTutorialPage :: MonadWidget t m => TutorialPage -> Event t [EnsembleResponse]
  -> Editor t m (Event t EnsembleRequest)
runTutorialPage p responsesDown = do
  translatedText $ tutorialPageTitle p
  viewWidget responsesDown $ tutorialPageView p
