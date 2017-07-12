module Estuary.Types.Action where

import Data.Maybe (mapMaybe)
import Text.JSON
import Estuary.Utility (firstKey)
import Estuary.Types.Sited
import Estuary.Types.EditOrEval
import Estuary.Types.View

data Action v =
  Chat String String | -- name message
  ZoneAction (Sited Int (EditOrEval v)) |
  GetAllViews |
  View (Sited String View) | 
  Tempo Double Double Double | -- at(timepoint) beat(continuous index) cps
  TempoChange Double

instance JSON v => JSON (Action v) where
  showJSON (Chat name msg) = encJSDict [("Chat",name),("m",msg)]
  showJSON (ZoneAction z) = encJSDict [("ZoneAction",showJSON z)]
  showJSON GetAllViews = showJSON "GetAllViews"
  showJSON (View x) = encJSDict [("View",x)]
  showJSON (Tempo at beat cps) = encJSDict [("Tempo",showJSON cps),("at",showJSON at),("beat",showJSON beat)]
  showJSON (TempoChange cps) = encJSDict [("TempoChange",showJSON cps)]
  readJSON (JSObject x) | firstKey x == "Chat" = Chat <$> valFromObj "Chat" x <*> valFromObj "m" x
  readJSON (JSObject x) | firstKey x == "ZoneAction" = ZoneAction <$> valFromObj "ZoneAction" x
  readJSON (JSString x) | fromJSString x == "GetAllViews" = Ok GetAllViews
  readJSON (JSObject x) | firstKey x == "View" = View <$> valFromObj "View" x
  readJSON (JSObject x) | firstKey x == "Tempo" = Tempo <$> valFromObj "Tempo" x <*> valFromObj "at" x <*> valFromObj "beat" x
  readJSON (JSObject x) | firstKey x == "TempoChange" = TempoChange <$> valFromObj "TempoChange" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as Action: " ++ (show x)
  readJSON (JSString x) | otherwise = Error $ "Unable to parse JSString as Action: " ++ (show x)
  readJSON _ = Error "Unable to parse as Action"

justEditsInZone :: Int -> [Action a] -> [a]
justEditsInZone z1 = mapMaybe f
  where
    f (ZoneAction (Sited z2 (Edit a))) | z1 ==z2 = Just a
    f _ = Nothing

justChats :: [Action v] -> [(String,String)]
justChats = mapMaybe f
  where f (Chat x y) = Just (x,y)
        f _ = Nothing

justViews :: [Action v] -> [Sited String View]
justViews = mapMaybe f
  where f (View x) = Just x
        f _ = Nothing

