module Estuary.Protocol.JSON where

import Text.JSON


data Request a =
  Authenticate String |
  RequestSpaceList |
  JoinSpace String |
  LeaveSpace |
  CreateSpace String |
  SpaceRequest (InSpace (Action a))

instance JSON a => JSON (Request a) where
  showJSON (Authenticate p) = encJSDict [("Authenticate",p)]
  showJSON (RequestSpaceList) = toJSString "RequestSpaceList"
  showJSON (JoinSpace s) = encJSDict [("JoinSpace",s]
  showJSON (LeaveSpace) = toJSString "LeaveSpace"
  showJSON (CreateSpace s) = encJSDict [("CreateSpace",s)]
  showJSON (SpaceRequest s) = encJSDict [("SpaceRequest",showJSON s)]
  readJSON (JSObject x) | firstKey x == "Authenticate" = Authenticate <$> valFromObj x "Authenticate"
  readJSON (JSString x) | fromJSString x == "RequestSpaceList" = Ok RequestSpaceList
  readJSON (JSObject x) | firstKey x == "JoinSpace" = JoinSpace <$> valFromObj x "JoinSpace"
  readJSON (JSString x) | fromJSString x == "LeaveSpace" = Ok LeaveSpace
  readJSON (JSObject x) | firstKey x == "CreateSpace" = CreateSpace <$> valFromObj x "CreateSpace"
  readJSON (JSObject x) | firstKey x == "SpaceRequest" = SpaceRequest <$> valFromObj x "SpaceRequest"
  readJSON _ = Error "Unable to parse as Estuary.Protocol.JSON.Request"


data Response a =
  SpaceList [String] |
  SpaceResponse (InSpace (Action a))

instance JSON a => JSON (Response a) where
  showJSON (SpaceList xs) = encJSDict [("SpaceList",showJSON xs)]
  showJSON (SpaceResponse r) = encJSDict [("SpaceResponse",showJSON r)]
  readJSON _ = Error "Unable to parse as Estuary.Protocol.JSON.Request"


data InSpace a = InSpace String a

instance JSON a => JSON (InSpace a) where
  showJSON (InSpace s x) = encJSDict [("InSpace",showJSON s),("x",showJSON x)]
  readJSON (JSObject x) | firstKey x == "InSpace" = InSpace <$> valFromObj x "InSpace" <$> valFromObj x "x"
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
  readJSON (JSObject x) | firstKey x == "Chat" = Chat <$> valFromObj x "Chat" <*> valFromObj x "m"
  readJSON (JSObject x) | firstKey x == "Edit" = Edit <$> valFromObj x "Edit" <*> valFromObj x "v"
  readJSON (JSObject x) | firstKey x == "Eval" = Eval <$> valFromObj x "Eval" <*> valFromObj x "v"
  readJSON (JSObject x) | firstKey x == "Tempo" = Tempo <$> valFromObj x "Tempo" <*> valFromObj x "at" <*> valFromObj x "beat"
  readJSON (JSObject x) | firstKey x == "TempoChange" = TempoChange <$> valFromObj x "TempoChange"
  readJSON _ = Error "Unable to parse as Estuary.Protocol.JSON.Action"


matchesNumber :: Int -> Action v -> Bool
matchesNumber z1 (Edit z2 _) = z1 == z2
matchesNumber z1 (Eval z2 _) = z1 == z2
matchesNumber _ _ = False

matchesZone = matchesNumber

isChat :: Action v -> Bool
isChat (Chat _ _) = True
isChat _ = False

isCps :: Action v -> Bool
isCps (Tempo _ _ _) = True
isCps _ = False

getCps :: Action v -> Maybe Double
getCps (Tempo _ _ x) = Just x
getCps _ = Nothing


type ServerRequest = Request (Action ZoneValue)

type ServerResponse = Response (Action ZoneValue)

data ZoneValue =
  Structure TransformedPattern |
  EvaluableText String |
  LabelText String |

instance JSON ZoneValue where
  showJSON (Structure x) = encJSDict [("Structure",x)]
  showJSON (EvaluableText x) = encJSDict [("EvaluableText",x)]
  showJSON (LabelText x) = encJSDict [("LabelText",x)]

isEstuaryEdit :: Action ZoneValue -> Bool
isEstuaryEdit (Edit _ (Structure _)) = True
isEstuaryEdit _ = False

isTextEdit :: Action ZoneValue -> Bool
isTextEdit (Edit _ (EvaluableText _)) = True
isTextEdit _ = False

isLabelEdit :: Action ZoneValue -> Bool
isLabelEdit (Edit _ (LabelTexxt _)) = True
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
