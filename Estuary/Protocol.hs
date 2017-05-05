module Estuary.Protocol where

import Text.JSON
import Estuary.Tidal.Types

data EstuaryProtocol =
  EstuaryEdit String Int TransformedPattern |
  TextEdit String Int String |
  TextEval String Int String |
  Chat String String String
  deriving (Show)

instance JSON EstuaryProtocol where
  showJSON (EstuaryEdit password n code) = encJSDict [("EstuaryEdit",showJSON n),("password",password),("code",showJSON code)]
  showJSON (TextEdit password n code) = encJSDict [("TextEdit",showJSON n),("password",password),("code",code)]
  showJSON (TextEval password n code) = encJSDict [("TextEval",showJSON n),("password",password),("code",code)]
  showJSON (Chat password name msg) = encJSDict [("Chat",msg),("password",password),("name",name)]
  readJSON (JSObject x) | firstKey x == "TextEdit" = EstuaryEdit <$> valFromObj "password" x <*> valFromObject "EstuaryEdit" x <*> valFromObject "code" x
  readJSON (JSObject x) | firstKey x == "TextEdit" = TextEdit <$> valFromObj "password" x <*> valFromObject "TextEdit" x <*> valFromObject "code" x
  readJSON (JSObject x) | firstKey x == "TextEdit" = TextEval <$> valFromObj "password" x <*> valFromObject "TextEval" x <*> valFromObject "code" x
  readJSON (JSObject x) | firstKey x == "Chat" = Chat <$> valFromObj "password" x <*> valFromObject "name" x <*> valFromObject "Chat" x
  readJSON _ = Error "Unable to parse as EstuaryProtocol"
