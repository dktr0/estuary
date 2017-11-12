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
  ListViews |
  GetView String |
  PublishView (Sited String View) |
  DeleteView String |
  TempoChange Double |
  GetEnsembleClientCount
  deriving (Eq)

instance JSON v => JSON (EnsembleRequest v) where
  showJSON (AuthenticateInEnsemble s) = encJSDict [("AuthenticateInEnsemble",s)]
  showJSON (SendChat name msg) = encJSDict [("SendChat",name),("m",msg)]
  showJSON (ZoneRequest z) = encJSDict [("ZoneRequest",showJSON z)]
  showJSON ListViews = showJSON "ListViews"
  showJSON (GetView x) = encJSDict [("GetView",x)]
  showJSON (PublishView x) = encJSDict [("PublishView",x)]
  showJSON (DeleteView x) = encJSDict [("DeleteView",x)]
  showJSON (TempoChange cps) = encJSDict [("TempoChange",showJSON cps)]
  showJSON GetEnsembleClientCount = showJSON "GetEnsembleClientCount"
  readJSON (JSObject x) | firstKey x == "AuthenticateInEnsemble" = AuthenticateInEnsemble <$> valFromObj "AuthenticateInEnsemble" x
  readJSON (JSObject x) | firstKey x == "SendChat" = SendChat <$> valFromObj "SendChat" x <*> valFromObj "m" x
  readJSON (JSObject x) | firstKey x == "ZoneRequest" = ZoneRequest <$> valFromObj "ZoneRequest" x
  readJSON (JSString x) | fromJSString x == "ListViews" = Ok ListViews
  readJSON (JSObject x) | firstKey x == "GetView" = GetView <$> valFromObj "GetView" x
  readJSON (JSObject x) | firstKey x == "PublishView" = PublishView <$> valFromObj "PublishView" x
  readJSON (JSObject x) | firstKey x == "DeleteView" = DeleteView <$> valFromObj "DeleteView" x
  readJSON (JSObject x) | firstKey x == "TempoChange" = TempoChange <$> valFromObj "TempoChange" x
  readJSON (JSString x) | fromJSString x == "GetEnsembleClientCount" = Ok GetEnsembleClientCount
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as EnsembleRequest: " ++ (show x)
  readJSON (JSString x) | otherwise = Error $ "Unable to parse JSString as EnsembleRequest: " ++ (show x)
  readJSON _ = Error "Unable to parse as EnsembleRequest"
