module Estuary.Types.EnsembleState where

import Data.Map
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.Definition
import Estuary.Types.View
import Estuary.Types.Sited
import qualified Estuary.Types.Terminal as Terminal

data EnsembleState = EnsembleState {
  ensembleName :: String,
  userHandle :: String,
  publishedViews :: Map String View,
  activeView :: View,
  activeViewName :: String
}

newEnsembleState :: String -> EnsembleState
newEnsembleState x = EnsembleState {
  ensembleName = x,
  userHandle = "",
  publishedViews = empty,
  activeView = defaultView,
  activeViewName = ""
}

commandsToStateChanges :: Terminal.Command -> EnsembleState -> EnsembleState
commandsToStateChanges _ es = es

responsesToStateChanges :: EnsembleResponse Definition -> EnsembleState -> EnsembleState
responsesToStateChanges _ es = es

commandsToRequests :: EnsembleState -> Terminal.Command -> Maybe (EnsembleRequest Definition)
commandsToRequests es (Terminal.Chat x) = Just (SendChat (userHandle es) x)
commandsToRequests es Terminal.ListViews = Just ListViews
commandsToRequests es (Terminal.GetView x) = Just (GetView x)
commandsToRequests es (Terminal.PublishView x) = Just (PublishView (Sited x (activeView es)))
commandsToRequests es (Terminal.DeleteView x) = Just (DeleteView x)
commandsToRequests _ _ = Nothing
