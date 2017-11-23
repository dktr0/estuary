module Estuary.Types.EnsembleState where

import Data.Map
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.Definition
import Estuary.Types.View
import Estuary.Types.Sited
import Estuary.Types.EditOrEval
import qualified Estuary.Types.Terminal as Terminal

data EnsembleState = EnsembleState {
  ensembleName :: String,
  userHandle :: String,
  zones :: Map Int Definition,
  publishedViews :: Map String View,
  defaultView :: View,
  activeView :: View,
  activeViewName :: String
}

newEnsembleState :: String -> EnsembleState
newEnsembleState x = EnsembleState {
  ensembleName = x,
  userHandle = "",
  zones = empty,
  publishedViews = empty,
  defaultView = standardView,
  activeView = standardView,
  activeViewName = ""
}

commandsToStateChanges :: Terminal.Command -> EnsembleState -> EnsembleState
commandsToStateChanges (Terminal.SetView v) es = es { activeView = v, activeViewName = "" }
commandsToStateChanges Terminal.StandardView es = es { activeView = standardView, activeViewName = "" }
commandsToStateChanges Terminal.DefaultView es = es { activeView = defaultView es }
commandsToStateChanges (Terminal.ActiveView x) es = es { activeView = findWithDefault emptyView x (publishedViews es),
  activeViewName = x }
commandsToStateChanges (Terminal.PublishView x) es = es { publishedViews = newViews, activeViewName = x }
  where newViews = insert x (activeView es) (publishedViews es)
commandsToStateChanges Terminal.PublishDefaultView es = es { defaultView = activeView es }
commandsToStateChanges (Terminal.DeleteView x) es = es { publishedViews = delete x (publishedViews es) }
commandsToStateChanges _ es = es

responsesToStateChanges :: EnsembleResponse Definition -> EnsembleState -> EnsembleState
responsesToStateChanges (ZoneResponse (Sited n (Edit v))) es = es { zones = newZones }
  where newZones = insert n v (zones es)
responsesToStateChanges (View (Sited s v)) es = es { publishedViews = newViews }
  where newViews = insert s v (publishedViews es)
responsesToStateChanges (DefaultView v) es = es { defaultView = v }
responsesToStateChanges _ es = es

commandsToRequests :: EnsembleState -> Terminal.Command -> Maybe (EnsembleRequest Definition)
commandsToRequests es (Terminal.PublishView x) = Just (PublishView (Sited x (activeView es)))
commandsToRequests es (Terminal.PublishDefaultView) = Just (PublishDefaultView (activeView es))
commandsToRequests es (Terminal.GetView x) = Just (GetView x)
commandsToRequests es Terminal.ListViews = Just ListViews
commandsToRequests es (Terminal.DeleteView x) = Just (DeleteView x)
commandsToRequests es (Terminal.Chat x) = Just (SendChat (userHandle es) x)
commandsToRequests _ _ = Nothing
