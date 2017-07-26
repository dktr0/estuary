module Estuary.Types.EnsembleRequest where

import Text.JSON
import Estuary.Utility (firstKey)
import Estuary.Types.Sited
import Estuary.Types.EditOrEval
import Estuary.Types.View

data EnsembleRequest v =
  AuthenticateInEnsemble String |
  SendChat String String | -- name message
  ZoneRequest (Sited Int (EditOrEval v)) |
  GetViews |
  SetView (Sited String View) |
  TempoChange Double |
  GetEnsembleClientCount
  
instance JSON v => JSON (EnsembleRequest v) where
  showJSON (AuthenticateInEnsemble s) = encJSDict [("AuthenticateInEnsemble",s)]
  showJSON (SendChat name msg) = encJSDict [("SendChat",name),("m",msg)]
  showJSON (ZoneRequest z) = encJSDict [("ZoneRequest",showJSON z)]
  showJSON GetViews = showJSON "GetViews"
  showJSON (SetView x) = encJSDict [("SetView",x)]
  showJSON (TempoChange cps) = encJSDict [("TempoChange",showJSON cps)]
  showJSON GetEnsembleClientCount = showJSON "GetEnsembleClientCount"
  readJSON (JSObject x) | firstKey x == "AuthenticateInEnsemble" = AuthenticateInEnsemble <$> valFromObj "AuthenticateInEnsemble" x
  readJSON (JSObject x) | firstKey x == "SendChat" = SendChat <$> valFromObj "SendChat" x <*> valFromObj "m" x
  readJSON (JSObject x) | firstKey x == "ZoneRequest" = ZoneRequest <$> valFromObj "ZoneRequest" x
  readJSON (JSString x) | fromJSString x == "GetViews" = Ok GetViews
  readJSON (JSObject x) | firstKey x == "SetView" = SetView <$> valFromObj "SetView" x
  readJSON (JSObject x) | firstKey x == "TempoChange" = TempoChange <$> valFromObj "TempoChange" x
  readJSON (JSString x) | fromJSString x == "GetEnsembleClientCount" = Ok GetEnsembleClientCount
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as EnsembleRequest: " ++ (show x)
  readJSON (JSString x) | otherwise = Error $ "Unable to parse JSString as EnsembleRequest: " ++ (show x)
  readJSON _ = Error "Unable to parse as EnsembleRequest"

