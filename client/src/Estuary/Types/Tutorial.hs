module Estuary.Types.Tutorial where

import Data.Sequence
import Estuary.Types.TranslatedText
import Estuary.Types.View

data TutorialPage = TutorialPage {
  tutorialPageTitle :: TranslatedText,
  tutorialPageView :: View
  }

data Tutorial = Tutorial {
  tutorialTitle :: TranslatedText,
  tutorialPages :: Seq TutorialPage
  }
