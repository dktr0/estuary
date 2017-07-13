module Estuary.Types.Request where

import Text.JSON
import Estuary.Utility (firstKey)
import Estuary.Types.Sited
import Estuary.Types.Action
import Estuary.Types.Definition

type ServerRequest = Request Definition

data Request a =
  Authenticate String |
  RequestSpaceList |
  JoinSpace String |
  LeaveSpace |
  CreateSpace String |
  SpaceRequest (Sited String (Action a)) |
  RequestServerClientCount 

instance JSON a => JSON (Request a) where
  showJSON (Authenticate p) = encJSDict [("Authenticate",p)]
  showJSON (RequestSpaceList) = showJSON "RequestSpaceList"
  showJSON (JoinSpace s) = encJSDict [("JoinSpace",s)]
  showJSON (LeaveSpace) = showJSON "LeaveSpace"
  showJSON (CreateSpace s) = encJSDict [("CreateSpace",s)]
  showJSON (SpaceRequest s) = encJSDict [("SpaceRequest",showJSON s)]
  showJSON (RequestServerClientCount) = showJSON "RequestServerClientCount"
  readJSON (JSObject x) | firstKey x == "Authenticate" = Authenticate <$> valFromObj "Authenticate" x
  readJSON (JSString x) | fromJSString x == "RequestSpaceList" = Ok RequestSpaceList
  readJSON (JSObject x) | firstKey x == "JoinSpace" = JoinSpace <$> valFromObj "JoinSpace" x
  readJSON (JSString x) | fromJSString x == "LeaveSpace" = Ok LeaveSpace
  readJSON (JSObject x) | firstKey x == "CreateSpace" = CreateSpace <$> valFromObj "CreateSpace" x
  readJSON (JSObject x) | firstKey x == "SpaceRequest" = SpaceRequest <$> valFromObj "SpaceRequest" x
  readJSON (JSString x) | fromJSString x == "RequestServerClientCount" = Ok RequestServerClientCount
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSOBject as Estuary.Protocol.JSON.Request: " ++ (show x)
  readJSON (JSString x) | otherwise = Error $ "Unable to parse JSString as Estuary.Protocol.JSON.Request: " ++ (show x)
  readJSON _ = Error "Unable to parse as Estuary.Protocol.JSON.Request (neither JSOBject nor JSString)"
