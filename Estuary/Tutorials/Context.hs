module Estuary.Tutorials.Context (tutorials) where

import Data.Map as M

import Estuary.Tutorials.IntroTidalText (introTidalText)
import Estuary.Tutorials.Tutorial

-- To be populated once tutorials exist
tutorials:: Map TutorialId Tutorial
tutorials = M.fromList $ fmap (\x->(tutorialId x,x)) [introTidalText]
