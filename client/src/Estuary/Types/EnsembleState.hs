module Estuary.Types.EnsembleState where

import Data.Map
import qualified Data.IntMap.Strict as IntMap
import Data.Time
import Data.Time.Clock.POSIX
import Data.Maybe

import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.Definition
import Estuary.Types.View
import Estuary.Types.Sited
import qualified Estuary.Types.Terminal as Terminal
import Estuary.Types.Tempo
import Estuary.Types.Hint
import Estuary.Types.ViewsParser
import Estuary.Render.AudioContext

data EnsembleState = EnsembleState {
  ensembleName :: String,
  userHandle :: String,
  zones :: IntMap.IntMap Definition,
  publishedViews :: Map String View,
  defaultView :: View,
  customView :: View,
  activeView :: Maybe String -- Nothing = defaultView, Just "" = CustomView, Just x = from publishedViews
}

soloEnsembleName :: String
soloEnsembleName = ""

commandToHint :: EnsembleState -> Terminal.Command -> Maybe Hint
commandToHint es (Terminal.DumpView) = Just $ LogMessage $ dumpView (currentView es)
commandToHint _ _ = Nothing

currentView :: EnsembleState -> View
currentView es | isNothing (activeView es) = defaultView es
currentView es | activeView es == Just "" = customView es
currentView es | otherwise = findWithDefault (Views []) x (publishedViews es)
  where x = fromJust $ activeView es

newEnsembleState :: String -> EnsembleState
newEnsembleState x = EnsembleState {
  ensembleName = x,
  userHandle = "",
  zones = IntMap.empty,
  publishedViews = empty,
  defaultView = emptyView,
  customView = emptyView,
  activeView = Nothing
}

getActiveView :: EnsembleState -> View
getActiveView e = f (activeView e)
  where f Nothing = defaultView e
        f (Just "") = customView e
        f (Just x) = findWithDefault emptyView x (publishedViews e)

commandsToStateChanges :: Terminal.Command -> EnsembleState -> EnsembleState
commandsToStateChanges (Terminal.SetView v) es = es { customView = v, activeView = Just "" }
commandsToStateChanges Terminal.StandardView es = es { customView = standardView, activeView = Just "" }
commandsToStateChanges (Terminal.PresetView v) es = es { customView = presetView v, activeView = Just ""}
commandsToStateChanges Terminal.DefaultView es = es { activeView = Nothing }
commandsToStateChanges (Terminal.ActiveView x) es = es { activeView = Just x }
commandsToStateChanges (Terminal.PublishView x) es = es { publishedViews = newViews, activeView = Just x }
  where newViews = insert x (getActiveView es) (publishedViews es)
commandsToStateChanges Terminal.PublishDefaultView es = es { defaultView = getActiveView es }
commandsToStateChanges (Terminal.DeleteView x) es = es { publishedViews = delete x (publishedViews es) }
commandsToStateChanges _ es = es

requestsToStateChanges :: EnsembleRequest -> EnsembleState -> EnsembleState
requestsToStateChanges (ZoneRequest n x) es = es { zones = IntMap.insert n x (zones es) }
requestsToStateChanges _ es = es

responsesToStateChanges :: EnsembleResponse -> EnsembleState -> EnsembleState
responsesToStateChanges (ZoneResponse n v) es = es { zones = newZones }
  where newZones = IntMap.insert n v (zones es)
responsesToStateChanges (View s v) es = es { publishedViews = newViews }
  where newViews = insert s v (publishedViews es)
responsesToStateChanges (DefaultView v) es = es { defaultView = v }
responsesToStateChanges _ es = es

commandsToRequests :: EnsembleState -> Terminal.Command -> Maybe EnsembleRequest
commandsToRequests es (Terminal.PublishView x) = Just (PublishView x (getActiveView es))
commandsToRequests es (Terminal.PublishDefaultView) = Just (PublishDefaultView (getActiveView es))
commandsToRequests es (Terminal.GetView x) = Just (GetView x)
commandsToRequests es Terminal.ListViews = Just ListViews
commandsToRequests es (Terminal.DeleteView x) = Just (DeleteView x)
commandsToRequests es (Terminal.Chat x) = Just (SendChat (userHandle es) x)
commandsToRequests _ _ = Nothing

messageForEnsembleResponse :: EnsembleResponse -> Maybe String
messageForEnsembleResponse (Chat name msg) = Just $ name ++ " chats: " ++ msg
messageForEnsembleResponse (ViewList xs) = Just $ "Views: " ++ (show xs)
messageForEnsembleResponse (View x _) = Just $ "received view " ++ x
messageForEnsembleResponse (NewTempo t _) = Just $ "received new tempo " ++ (show (cps t))
messageForEnsembleResponse _ = Nothing
