module Estuary.Types.Tutorial

import Estuary.Types.View
import Estuary.Types.Definition

type TutorialPage = (View, DefinitionMap)

data TutorialId = TidalStructureEditting | TidalTextEditing deriving (Show, Eq)

data Tutorial i p = Tutorial {
  tutorialId::TutorialId,
  pages:: Map Language [TutorialPage],
}
