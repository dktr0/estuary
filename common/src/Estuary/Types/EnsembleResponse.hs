module Estuary.Types.EnsembleResponse where

import Data.Maybe (mapMaybe)
import Data.Ratio
import Text.JSON
import Estuary.Utility (firstKey)
import Estuary.Types.Sited
import Estuary.Types.EditOrEval
import Estuary.Types.View

data EnsembleResponse v =
  Chat String String | -- name message
  ZoneResponse (Sited Int (EditOrEval v)) |
  ViewList [String] |
  View (Sited String View) |
  DefaultView View |
  Tempo Double Rational Double | --  cps at(timepoint) beat(continuous index)
  EnsembleClientCount Int

instance JSON v => JSON (EnsembleResponse v) where
  showJSON (Chat name msg) = encJSDict [("Chat",name),("m",msg)]
  showJSON (ZoneResponse z) = encJSDict [("ZoneResponse",showJSON z)]
  showJSON (ViewList x) = encJSDict [("ViewList",x)]
  showJSON (View x) = encJSDict [("View",x)]
  showJSON (DefaultView x) = encJSDict [("DefaultView",x)]
  showJSON (Tempo cps at beat) = encJSDict [("Tempo",showJSON cps),("atN",showJSON $ numerator at),("atD",showJSON $ denominator at),("beat",showJSON beat)]
  showJSON (EnsembleClientCount x) = encJSDict [("EnsembleClientCount",x)]
  readJSON (JSObject x) | firstKey x == "Chat" = Chat <$> valFromObj "Chat" x <*> valFromObj "m" x
  readJSON (JSObject x) | firstKey x == "ZoneResponse" = ZoneResponse <$> valFromObj "ZoneResponse" x
  readJSON (JSObject x) | firstKey x == "ViewList" = ViewList <$> valFromObj "ViewList" x
  readJSON (JSObject x) | firstKey x == "View" = View <$> valFromObj "View" x
  readJSON (JSObject x) | firstKey x == "DefaultView" = DefaultView <$> valFromObj "DefaultView" x
  readJSON (JSObject x) | firstKey x == "Tempo" = Tempo <$> valFromObj "Tempo" x <*> at' <*> valFromObj "beat" x
    where at' = (%) <$> valFromObj "atN" x <*> valFromObj "atD" x
  readJSON (JSObject x) | firstKey x == "EnsembleClientCount" = EnsembleClientCount <$> valFromObj "EnsembleClientCount" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as EnsembleResponse: " ++ (show x)
  readJSON _ = Error "Unable to parse non-JSObject as EnsembleResponse"

justEditsInZone :: Int -> [EnsembleResponse a] -> [a]
justEditsInZone z1 = mapMaybe f
  where
    f (ZoneResponse (Sited z2 (Edit a))) | z1 ==z2 = Just a
    f _ = Nothing

justChats :: [EnsembleResponse v] -> [(String,String)]
justChats = mapMaybe f
  where f (Chat x y) = Just (x,y)
        f _ = Nothing

justViews :: [EnsembleResponse v] -> [Sited String View]
justViews = mapMaybe f
  where f (View x) = Just x
        f _ = Nothing
