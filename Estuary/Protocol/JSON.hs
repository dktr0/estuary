module Estuary.Protocol.JSON where

import Text.JSON
import Estuary.Tidal.Types
import Data.Time.Clock.POSIX

data EstuaryProtocol =
  EstuaryEdit String Int TransformedPattern |
  TextEdit String Int String |
  TextEval String Int String |
  LabelEdit String Int String |
  Chat String String String | -- password name msg
  Tempo String Double Double Double | -- password at beat cps
  TempoChange String Double |
  ProtocolError String
  deriving (Show)

instance JSON EstuaryProtocol where
  showJSON (EstuaryEdit password n code) = encJSDict [("EEdit",showJSON n),("p",showJSON password),("c",showJSON code)]
  showJSON (TextEdit password n code) = encJSDict [("TEdit",showJSON n),("p",showJSON password),("c",showJSON code)]
  showJSON (TextEval password n code) = encJSDict [("TEval",showJSON n),("p",showJSON password),("c",showJSON code)]
  showJSON (LabelEdit password n t) = encJSDict [("LEdit",showJSON n),("p",showJSON password),("t",showJSON t)]
  showJSON (Chat password name msg) = encJSDict [("Chat",msg),("p",password),("n",name)]
  showJSON (Tempo password at beat cps) = encJSDict [("Tempo",showJSON cps),("p",showJSON password),("at",showJSON at),("beat",showJSON beat)]
  showJSON (TempoChange password cps) = encJSDict [("Change",showJSON cps),("p",showJSON password)]
  showJSON (ProtocolError msg) = encJSDict [("Err",msg)]
  readJSON (JSObject x) | firstKey x == "EEdit" = EstuaryEdit <$> valFromObj "p" x <*> valFromObj "EEdit" x <*> valFromObj "c" x
  readJSON (JSObject x) | firstKey x == "TEdit" = TextEdit <$> valFromObj "p" x <*> valFromObj "TEdit" x <*> valFromObj "c" x
  readJSON (JSObject x) | firstKey x == "TEval" = TextEval <$> valFromObj "p" x <*> valFromObj "TEval" x <*> valFromObj "c" x
  readJSON (JSObject x) | firstKey x == "LEdit" = LabelEdit <$> valFromObj "p" x <*> valFromObj "Edit" x <*> valFromObj "t" x
  readJSON (JSObject x) | firstKey x == "Chat" = Chat <$> valFromObj "p" x <*> valFromObj "n" x <*> valFromObj "Chat" x
  readJSON (JSObject x) | firstKey x == "Tempo" = Tempo <$> valFromObj "p" x <*> valFromObj "at" x <*> valFromObj "beat" x <*> valFromObj "Tempo" x
  readJSON (JSObject x) | firstKey x == "Change" = TempoChange <$> valFromObj "p" x <*> valFromObj "Change" x
  readJSON (JSObject x) | firstKey x == "Err" = ProtocolError <$> valFromObj "Err" x
  readJSON _ = Error "Unable to parse as EstuaryProtocol"

setPassword :: String -> EstuaryProtocol -> EstuaryProtocol
setPassword x (EstuaryEdit _ n c) = EstuaryEdit x n c
setPassword x (TextEdit _ n c) = TextEdit x n c
setPassword x (TextEval _ n c) = TextEval x n c
setPassword x (LabelEdit _ n t) = LabelEdit x n t
setPassword x (Chat _ m n) = Chat x m n
setPassword x (Tempo _ a b c) = Tempo x a b c
setPassword x (TempoChange _ a) = TempoChange x a

matchesNumber :: Int -> EstuaryProtocol -> Bool
matchesNumber n1 (EstuaryEdit _ n2 _) = n1 == n2
matchesNumber n1 (TextEdit _ n2 _) = n1 == n2
matchesNumber n1 (TextEval _ n2 _) = n1 == n2
matchesNumber n1 (LabelEdit _ n2 _) = n1 == n2
matchesNumber _ _ = False

isEstuaryEdit :: EstuaryProtocol -> Bool
isEstuaryEdit (EstuaryEdit _ _ _) = True
isEstuaryEdit _ = False

isTextEdit :: EstuaryProtocol -> Bool
isTextEdit (TextEdit _ _ _) = True
isTextEdit _ = False

isLabelEdit :: EstuaryProtocol -> Bool
isLabelEdit (LabelEdit _ _ _) = True
isLabelEdit _ = False

isChat :: EstuaryProtocol -> Bool
isChat (Chat _ _ _) = True
isChat _ = False

justEstuaryCode :: EstuaryProtocol -> TransformedPattern
justEstuaryCode (EstuaryEdit _ _ x) = x
justEstuaryCode _ = error "can't get estuary code from non EstuaryEdit"

justTextCode :: EstuaryProtocol -> String
justTextCode (TextEdit _ _ x) = x
justTextCode (TextEval _ _ x) = x
justTextCode _ = error "can't get text code from non TextEdit or TextEval"

justText :: EstuaryProtocol -> String
justText (TextEdit _ _ x) = x
justText (TextEval _ _ x) = x
justText (LabelEdit _ _ x) = x
justText (Chat _ _ x) = x
justText _ = error "can't get text from non TextEdit/TextEval/LabelEdit"

isCps :: EstuaryProtocol -> Bool
isCps (Tempo _ _ _ _) = True
isCps _ = False

getCps :: EstuaryProtocol -> Maybe Double
getCps (Tempo _ _ _ x) = Just x
getCps _ = Nothing

-- the function lastOrNothing is useful with fmapMaybe on events containing lists of EstuaryProtocol items
-- so that no event is fired when the list is empty

lastOrNothing :: [a] -> Maybe a
lastOrNothing [] = Nothing
lastOrNothing xs = Just (last xs)

