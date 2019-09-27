{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.Tutorial (runTutorial) where

import Data.Sequence as Sequence
import Estuary.Types.Tutorial
import Estuary.Types.EnsembleResponse
import Estuary.Types.EnsembleRequest
import Estuary.Types.Editor

runTutorial :: MonadWidget t m => Tutorial -> Event t [EnsembleResponse]
  -> Editor t m (Event t EnsembleRequest)
runTutorial t responsesDown = mdo
  curPage <- holdDyn 0 $ leftmost pageNavEvents
  let nPages = Sequence.length $ tutorialPages t
  let prevPage = fmap (\x -> max (x-1) 0) curPage
  let nextPage = fmap (\x -> min (x+1) (nPages-1)) curPage
  translatedText $ tutorialTitle t
  navPrev <- liftM (tagPromptlyDyn prevPage) $ button "prev" -- should be translated + active/inactive
  text, disactivated when on first page
  let titleOfCurrentPage = fmap ( . index (tutorialPages t)) curPage
  translatedText $ titleOfCurrentPage
  navNext <- liftM (tagPromptlyDyn nextPage) $ button "next" -- should be translated + active/inactive
  let pageNavEvents = leftmost [navPrev,navNext]
  let initialPage = runTutorialPage (lookup 0 $ tutorialPages t) responsesDown
  let builder = fmap (\x -> runTutorialPage (index (tutorialPages t) x) responsesDown) pageNavEvents
  thePage <- widgetHold initialPage builder


runTutorialPage :: MonadWidget t m => TutorialPage -> Event t [EnsembleResponse]
  -> Editor t m (Event t EnsembleRequest)
runTutorialPage p i responsesDown = do
