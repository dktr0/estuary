module Estuary.Types.Action where

import Data.Maybe (mapMaybe)
import Text.JSON
import Estuary.Utility (firstKey)
import Estuary.Types.Sited
import Estuary.Types.EditOrEval

data Action v =
  Chat String String | -- name message
  ZoneAction (Sited Int (EditOrEval v)) |
  Tempo Double Double Double | -- at(timepoint) beat(continuous index) cps
  TempoChange Double

instance JSON v => JSON (Action v) where
  showJSON (Chat name msg) = encJSDict [("Chat",name),("m",msg)]
  showJSON (ZoneAction z) = encJSDict [("ZoneAction",showJSON z)]
  showJSON (Tempo at beat cps) = encJSDict [("Tempo",showJSON cps),("at",showJSON at),("beat",showJSON beat)]
  showJSON (TempoChange cps) = encJSDict [("TempoChange",showJSON cps)]
  readJSON (JSObject x) | firstKey x == "Chat" = Chat <$> valFromObj "Chat" x <*> valFromObj "m" x
  readJSON (JSObject x) | firstKey x == "ZoneAction" = ZoneAction <$> valFromObj "ZoneAction" x
  readJSON (JSObject x) | firstKey x == "Tempo" = Tempo <$> valFromObj "Tempo" x <*> valFromObj "at" x <*> valFromObj "beat" x
  readJSON (JSObject x) | firstKey x == "TempoChange" = TempoChange <$> valFromObj "TempoChange" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as Action: " ++ (show x)
  readJSON _ = Error "Unable to parse as Estuary.Protocol.JSON.Action"

justEditsInZone :: Int -> [Action a] -> [a]
justEditsInZone z1 = mapMaybe f
  where
    f (ZoneAction (Sited z2 (Edit a))) | z1 ==z2 = Just a
    f _ = Nothing

justChats :: [Action v] -> [(String,String)]
justChats = mapMaybe f
  where f (Chat x y) = Just (x,y)
        f _ = Nothing
