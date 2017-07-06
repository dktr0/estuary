module Estuary.Protocol.JSON where

import Estuary.Tidal.Types
import Text.JSON
import Data.Maybe


data Request a =
  Authenticate String |
  RequestSpaceList |
  JoinSpace String |
  LeaveSpace |
  CreateSpace String |
  SpaceRequest (InSpace (Action a))

instance JSON a => JSON (Request a) where
  showJSON (Authenticate p) = encJSDict [("Authenticate",p)]
  showJSON (RequestSpaceList) = showJSON "RequestSpaceList"
  showJSON (JoinSpace s) = encJSDict [("JoinSpace",s)]
  showJSON (LeaveSpace) = showJSON "LeaveSpace"
  showJSON (CreateSpace s) = encJSDict [("CreateSpace",s)]
  showJSON (SpaceRequest s) = encJSDict [("SpaceRequest",showJSON s)]
  readJSON (JSObject x) | firstKey x == "Authenticate" = Authenticate <$> valFromObj "Authenticate" x
  readJSON (JSString x) | fromJSString x == "RequestSpaceList" = Ok RequestSpaceList
  readJSON (JSObject x) | firstKey x == "JoinSpace" = JoinSpace <$> valFromObj "JoinSpace" x
  readJSON (JSString x) | fromJSString x == "LeaveSpace" = Ok LeaveSpace
  readJSON (JSObject x) | firstKey x == "CreateSpace" = CreateSpace <$> valFromObj "CreateSpace" x
  readJSON (JSObject x) | firstKey x == "SpaceRequest" = SpaceRequest <$> valFromObj "SpaceRequest" x
  readJSON _ = Error "Unable to parse as Estuary.Protocol.JSON.Request"


data Response a =
  SpaceList [String] |
  SpaceResponse (InSpace (Action a))

instance JSON a => JSON (Response a) where
  showJSON (SpaceList xs) = encJSDict [("SpaceList",showJSON xs)]
  showJSON (SpaceResponse r) = encJSDict [("SpaceResponse",showJSON r)]
  readJSON (JSObject x) | firstKey x == "SpaceList" = SpaceList <$> valFromObj "SpaceList" x
  readJSON (JSObject x) | firstKey x == "SpaceResponse" = SpaceResponse <$> valFromObj "SpaceResponse" x
  readJSON _ = Error "Unable to parse as Estuary.Protocol.JSON.Request"

justActionsInSpace :: String -> [Response a] -> [Action a]
justActionsInSpace s1 = mapMaybe f
  where f (SpaceResponse (InSpace s2 a)) | s1 == s2 = Just a
        f _ = Nothing
 
data InSpace a = InSpace String a

instance JSON a => JSON (InSpace a) where
  showJSON (InSpace s x) = encJSDict [("InSpace",showJSON s),("x",showJSON x)]
  readJSON (JSObject x) | firstKey x == "InSpace" = InSpace <$> valFromObj "InSpace" x <*> valFromObj "x" x
  readJSON _ = Error "Unable to parse as Estuary.Protocol.JSON.InSpace"


type Zone = Int

data Action v =
  Chat String String | -- name message
  Edit Zone v |
  Eval Zone v |
  Tempo Double Double Double | -- at(timepoint) beat(continuous index) cps
  TempoChange Double

instance JSON v => JSON (Action v) where
  showJSON (Chat name msg) = encJSDict [("Chat",name),("m",msg)]
  showJSON (Edit z v) = encJSDict [("Edit",showJSON z),("v",showJSON v)]
  showJSON (Eval z v) = encJSDict [("Eval",showJSON z),("v",showJSON v)]
  showJSON (Tempo at beat cps) = encJSDict [("Tempo",showJSON cps),("at",showJSON at),("beat",showJSON beat)]
  showJSON (TempoChange cps) = encJSDict [("TempoChange",showJSON cps)]
  readJSON (JSObject x) | firstKey x == "Chat" = Chat <$> valFromObj "Chat" x <*> valFromObj "m" x
  readJSON (JSObject x) | firstKey x == "Edit" = Edit <$> valFromObj "Edit" x <*> valFromObj "v" x
  readJSON (JSObject x) | firstKey x == "Eval" = Eval <$> valFromObj "Eval" x <*> valFromObj "v" x
  readJSON (JSObject x) | firstKey x == "Tempo" = Tempo <$> valFromObj "Tempo" x <*> valFromObj "at" x <*> valFromObj "beat" x
  readJSON (JSObject x) | firstKey x == "TempoChange" = TempoChange <$> valFromObj "TempoChange" x
  readJSON _ = Error "Unable to parse as Estuary.Protocol.JSON.Action"

justEditsInZone :: Zone -> [Action a] -> [a]
justEditsInZone z1 = mapMaybe f
  where
    f (Edit z2 a) | z1 ==z2 = Just a
    f _ = Nothing

matchesNumber :: Int -> Action v -> Bool
matchesNumber z1 (Edit z2 _) = z1 == z2
matchesNumber z1 (Eval z2 _) = z1 == z2
matchesNumber _ _ = False

matchesZone = matchesNumber

isChat :: Action v -> Bool
isChat (Chat _ _) = True
isChat _ = False

justChats :: [Action v] -> [(String,String)]
justChats = mapMaybe f
  where f (Chat x y) = Just (x,y)
        f _ = Nothing

isCps :: Action v -> Bool
isCps (Tempo _ _ _) = True
isCps _ = False

getCps :: Action v -> Maybe Double
getCps (Tempo _ _ x) = Just x
getCps _ = Nothing


type ServerRequest = Request ZoneValue

type ServerResponse = Response ZoneValue

data ZoneValue =
  Structure TransformedPattern |
  EvaluableText String |
  LabelText String

instance JSON ZoneValue where
  showJSON (Structure x) = encJSDict [("Structure",x)]
  showJSON (EvaluableText x) = encJSDict [("EvaluableText",x)]
  showJSON (LabelText x) = encJSDict [("LabelText",x)]
  readJSON (JSObject x) | firstKey x == "Structure" = Structure <$> valFromObj "Structure" x
  readJSON (JSObject x) | firstKey x == "EvaluableText" = EvaluableText <$> valFromObj "EvaluableText" x
  readJSON (JSObject x) | firstKey x == "LabelText" = LabelText <$> valFromObj "LabelText" x
  readJSON _ = Error "Unable to parse as Estuary.Protocol.JSON.ZoneValue"

justStructures :: [ZoneValue] -> [TransformedPattern]
justStructures = mapMaybe f
  where f (Structure x) = Just x
        f _ = Nothing
 
justEvaluableTexts :: [ZoneValue] -> [String]
justEvaluableTexts = mapMaybe f
  where f (EvaluableText x) = Just x
        f _ = Nothing

justLabelTexts :: [ZoneValue] -> [String]
justLabelTexts = mapMaybe f
  where f (LabelText x) = Just x
        f _ = Nothing

isEstuaryEdit :: Action ZoneValue -> Bool
isEstuaryEdit (Edit _ (Structure _)) = True
isEstuaryEdit _ = False

isTextEdit :: Action ZoneValue -> Bool
isTextEdit (Edit _ (EvaluableText _)) = True
isTextEdit _ = False

isLabelEdit :: Action ZoneValue -> Bool
isLabelEdit (Edit _ (LabelText _)) = True
isLabelEdit _ = False

justEstuaryCode :: Action ZoneValue -> TransformedPattern
justEstuaryCode (Edit _ (Structure x)) = x
justEstuaryCode _ = error "can't get estuary code from non EstuaryEdit"

justTextCode :: Action ZoneValue -> String
justTextCode (Edit _ (EvaluableText x)) = x
justTextCode (Eval _ (EvaluableText x)) = x
justTextCode _ = error "can't get text code from non TextEdit or TextEval"

justText :: Action ZoneValue -> String
justText (Edit _ (EvaluableText x)) = x
justText (Eval _ (EvaluableText x)) = x
justText (Edit _ (LabelText x)) = x
justText (Chat _ x) = x
justText _ = error "can't get text from non TextEdit/TextEval/LabelEdit/Chat"



-- the function lastOrNothing is useful with fmapMaybe on events containing lists of EstuaryProtocol items
-- so that no event is fired when the list is empty

lastOrNothing :: [a] -> Maybe a
lastOrNothing [] = Nothing
lastOrNothing xs = Just (last xs)
