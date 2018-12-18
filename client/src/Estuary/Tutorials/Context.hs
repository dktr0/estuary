module Estuary.Tutorials.Context where

import Reflex
import Reflex.Dom

import Data.Map
import Estuary.Types.Definition
import Estuary.Types.Hint
import Estuary.Types.Context

import Estuary.Tutorials.Tutorial
import Estuary.Tutorials.IntroTidalText (introTidalText)

tutorials::MonadWidget t m => [Tutorial t m]
tutorials = [introTidalText]

tutorialMap::MonadWidget t m => Map TutorialId ((Dynamic t Context -> m (Dynamic t DefinitionMap, Event t Hint)))
tutorialMap = fromList $ fmap (\x -> (tutorialId x, widget x)) tutorials
