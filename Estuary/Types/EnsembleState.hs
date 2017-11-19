module Estuary.Types.EnsembleState where

import Data.Map

import Estuary.Types.View

data EnsembleState = EnsembleState {
  ensembleName :: String,
  handle :: String,
  publishedViews :: Map String View,
  activeView :: View,
  activeViewName :: String
}


commandsToRequests :: EnsembleState -> Command.TerminalCommand -> Maybe (EnsembleRequest Definition)
commandsToRequests es (Command.Chat x) = Just (SendChat (handle es) x)
commandsToRequests es Command.ListViews = Just ListViews
commandsToRequests es (Command.GetView x) = Just (GetView x)
commandsToRequests es (Command.PublishView x) = Just (PublishView (Sited x v))
commandsToRequests es (Command.DeleteView x) = Just (DeleteView x)
commandsToRequests _ _ = Nothing
