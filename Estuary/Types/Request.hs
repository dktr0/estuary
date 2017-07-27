module Estuary.Types.Request where

import Text.JSON
import Estuary.Utility (firstKey)
import Estuary.Types.Sited
import Estuary.Types.EnsembleRequest
import Estuary.Types.Definition

type ServerRequest = Request Definition

data Request a =
  Authenticate String |
  GetEnsembleList |
  JoinEnsemble String |
  LeaveEnsemble |
  CreateEnsemble String String | -- ensembleName ensemblePassword (or "" for no password)
  EnsembleRequest (Sited String (EnsembleRequest a)) |
  GetServerClientCount 

instance JSON a => JSON (Request a) where
  showJSON (Authenticate p) = encJSDict [("Authenticate",p)]
  showJSON (GetEnsembleList) = showJSON "GetEnsembleList"
  showJSON (JoinEnsemble s) = encJSDict [("JoinEnsemble",s)]
  showJSON (LeaveEnsemble) = showJSON "LeaveEnsemble"
  showJSON (CreateEnsemble name pwd) = encJSDict [("CreateEnsemble",showJSON name),("pwd",showJSON pwd)]
  showJSON (EnsembleRequest s) = encJSDict [("EnsembleRequest",showJSON s)]
  showJSON (GetServerClientCount) = showJSON "GetServerClientCount"
  readJSON (JSObject x) | firstKey x == "Authenticate" = Authenticate <$> valFromObj "Authenticate" x
  readJSON (JSString x) | fromJSString x == "GetEnsembleList" = Ok GetEnsembleList
  readJSON (JSObject x) | firstKey x == "JoinEnsemble" = JoinEnsemble <$> valFromObj "JoinEnsemble" x
  readJSON (JSString x) | fromJSString x == "LeaveEnsemble" = Ok LeaveEnsemble
  readJSON (JSObject x) | firstKey x == "CreateEnsemble" = CreateEnsemble <$> valFromObj "CreateEnsemble" x <*> valFromObj "pwd" x
  readJSON (JSObject x) | firstKey x == "EnsembleRequest" = EnsembleRequest <$> valFromObj "EnsembleRequest" x
  readJSON (JSString x) | fromJSString x == "GetServerClientCount" = Ok GetServerClientCount
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSOBject as Request: " ++ (show x)
  readJSON (JSString x) | otherwise = Error $ "Unable to parse JSString as Request: " ++ (show x)
  readJSON _ = Error "Unable to parse as Request (neither JSOBject nor JSString)"

