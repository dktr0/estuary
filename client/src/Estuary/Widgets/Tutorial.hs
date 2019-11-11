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

runTutorial :: MonadWidget t m => Tutorial -> Event t [EnsembleResponse]
  -> Editor t m (Event t EnsembleRequest)
runTutorial t responsesDown = mdo
  curPage <- liftR $ holdDyn 0 pageNavEvents
  let nPages = Seq.length $ tutorialPages t
  let prevPage = fmap (\x -> max (x-1) 0) curPage
  let nextPage = fmap (\x -> min (x+1) (nPages-1)) curPage
  translatedText $ tutorialTitle t
  navPrev <- liftR $ liftM (tagPromptlyDyn prevPage) $ button "prev" -- *** TODO: should be translated + active/inactive text, disactivated when on first page
  let titleOfCurrentPage = fmap (tutorialPageTitle . index (tutorialPages t)) curPage
  translatedDynText $ titleOfCurrentPage
  navNext <- liftR $ liftM (tagPromptlyDyn nextPage) $ button "next" -- *** TODO: should be translated + active/inactive
  let pageNavEvents = leftmost [navPrev,navNext]
  let initialPage = runTutorialPage (index (tutorialPages t) 0) responsesDown
  let builder = fmap (\x -> runTutorialPage (index (tutorialPages t) x) responsesDown) pageNavEvents
  thePage <- editorHold initialPage builder
  return never


runTutorialPage :: MonadWidget t m => TutorialPage -> Event t [EnsembleResponse]
  -> Editor t m (Event t EnsembleRequest)
runTutorialPage p responsesDown = return never -- placeholder
