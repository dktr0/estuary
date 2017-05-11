module Estuary.Protocol.JSON where

import Text.JSON
import Estuary.Tidal.Types
import Data.Time.Clock.POSIX

data EstuaryProtocol =
  EstuaryEdit String Int TransformedPattern |
  TextEdit String Int String |
  TextEval String Int String |
  Chat String String String |
  Tempo String POSIXTime Double Double |
  TempoChange String Double |
  ProtocolError String
  deriving (Show)

instance JSON EstuaryProtocol where
  showJSON (EstuaryEdit password n code) = encJSDict [("EstuaryEdit",showJSON n),("password",showJSON password),("code",showJSON code)]
  showJSON (TextEdit password n code) = encJSDict [("TextEdit",showJSON n),("password",showJSON password),("code",showJSON code)]
  showJSON (TextEval password n code) = encJSDict [("TextEval",showJSON n),("password",showJSON password),("code",showJSON code)]
  showJSON (Chat password name msg) = encJSDict [("Chat",msg),("password",password),("name",name)]
  showJSON (Tempo password at beat cps) = encJSDict [("Tempo",showJSON cps),("password",showJSON password),("at",showJSON (toRational at)),("beat",showJSON beat)]
  showJSON (TempoChange password cps) = encJSDict [("TempoChange",showJSON cps),("password",showJSON password)]
  showJSON (ProtocolError msg) = encJSDict [("ProtocolError",msg)]
  readJSON (JSObject x) | firstKey x == "EstuaryEdit" = EstuaryEdit <$> valFromObj "password" x <*> valFromObj "EstuaryEdit" x <*> valFromObj "code" x
  readJSON (JSObject x) | firstKey x == "TextEdit" = TextEdit <$> valFromObj "password" x <*> valFromObj "TextEdit" x <*> valFromObj "code" x
  readJSON (JSObject x) | firstKey x == "TextEdit" = TextEval <$> valFromObj "password" x <*> valFromObj "TextEval" x <*> valFromObj "code" x
  readJSON (JSObject x) | firstKey x == "Chat" = Chat <$> valFromObj "password" x <*> valFromObj "name" x <*> valFromObj "Chat" x
  readJSON (JSObject x) | firstKey x == "Tempo" = Tempo <$> valFromObj "password" x <*> (fromRational <$> valFromObj "at" x)  <*> valFromObj "beat" x <*> valFromObj "Tempo" x
  readJSON (JSObject x) | firstKey x == "TempoChange" = TempoChange <$> valFromObj "password" x <*> valFromObj "TempoChange" x
  readJSON (JSObject x) | firstKey x == "ProtocolError" = ProtocolError <$> valFromObj "ProtocolError" x
  readJSON _ = Error "Unable to parse as EstuaryProtocol"

setPassword :: String -> EstuaryProtocol -> EstuaryProtocol
setPassword x (EstuaryEdit _ n c) = EstuaryEdit x n c
setPassword x (TextEdit _ n c) = TextEdit x n c
setPassword x (TextEval _ n c) = TextEval x n c
setPassword x (Chat _ m n) = Chat x m n

matchesNumber :: Int -> EstuaryProtocol -> Bool
matchesNumber n1 (EstuaryEdit _ n2 _) = n1 == n2
matchesNumber n1 (TextEdit _ n2 _) = n1 == n2
matchesNumber n1 (TextEval _ n2 _) = n1 == n2
matchesNumber _ _ = False

isEstuaryEdit :: EstuaryProtocol -> Bool
isEstuaryEdit (EstuaryEdit _ _ _) = True
isEstuaryEdit _ = False

isTextEdit :: EstuaryProtocol -> Bool
isTextEdit (TextEdit _ _ _) = True
isTextEdit _ = False

justEstuaryCode :: EstuaryProtocol -> TransformedPattern
justEstuaryCode (EstuaryEdit _ _ x) = x
justEstuaryCode _ = error "can't get estuary code from non EstuaryEdit"

justTextCode :: EstuaryProtocol -> String
justTextCode (TextEdit _ _ x) = x
justTextCode (TextEval _ _ x) = x
justTextCode _ = error "can't get text code from non TextEdit or TextEval"
