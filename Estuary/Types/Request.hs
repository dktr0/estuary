module Estuary.Types.Request where

import Text.JSON
import Estuary.Utility (firstKey)
import Estuary.Types.Sited
import Estuary.Types.SpaceRequest
import Estuary.Types.Definition

type ServerRequest = Request Definition

data Request a =
  Authenticate String |
  RequestSpaceList |
  JoinSpace String |
  LeaveSpace |
  CreateSpace String |
  SpaceRequest (Sited String (SpaceRequest a)) |
  GetServerClientCount 

instance JSON a => JSON (Request a) where
  showJSON (Authenticate p) = encJSDict [("Authenticate",p)]
  showJSON (RequestSpaceList) = showJSON "RequestSpaceList"
  showJSON (JoinSpace s) = encJSDict [("JoinSpace",s)]
  showJSON (LeaveSpace) = showJSON "LeaveSpace"
  showJSON (CreateSpace s) = encJSDict [("CreateSpace",s)]
  showJSON (SpaceRequest s) = encJSDict [("SpaceRequest",showJSON s)]
  showJSON (GetServerClientCount) = showJSON "GetServerClientCount"
  readJSON (JSObject x) | firstKey x == "Authenticate" = Authenticate <$> valFromObj "Authenticate" x
  readJSON (JSString x) | fromJSString x == "RequestSpaceList" = Ok RequestSpaceList
  readJSON (JSObject x) | firstKey x == "JoinSpace" = JoinSpace <$> valFromObj "JoinSpace" x
  readJSON (JSString x) | fromJSString x == "LeaveSpace" = Ok LeaveSpace
  readJSON (JSObject x) | firstKey x == "CreateSpace" = CreateSpace <$> valFromObj "CreateSpace" x
  readJSON (JSObject x) | firstKey x == "SpaceRequest" = SpaceRequest <$> valFromObj "SpaceRequest" x
  readJSON (JSString x) | fromJSString x == "GetServerClientCount" = Ok GetServerClientCount
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSOBject as Request: " ++ (show x)
  readJSON (JSString x) | otherwise = Error $ "Unable to parse JSString as Request: " ++ (show x)
  readJSON _ = Error "Unable to parse as Request (neither JSOBject nor JSString)"

