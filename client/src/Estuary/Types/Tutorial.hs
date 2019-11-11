module Estuary.Types.Tutorial where

import Data.Sequence
import Estuary.Types.TranslatableText
import Estuary.Types.View

data TutorialPage = TutorialPage {
  tutorialPageTitle :: TranslatableText,
  tutorialPageView :: View
  }

data Tutorial = Tutorial {
  tutorialTitle :: TranslatableText,
  tutorialPages :: Seq TutorialPage
  }
