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
  showJSON (EstuaryEdit password n code) = encJSDict [("EstuaryEdit",showJSON n),("password",showJSON password),("code",showJSON code)]
  showJSON (TextEdit password n code) = encJSDict [("TextEdit",showJSON n),("password",showJSON password),("code",showJSON code)]
  showJSON (TextEval password n code) = encJSDict [("TextEval",showJSON n),("password",showJSON password),("code",showJSON code)]
  showJSON (Chat password name msg) = encJSDict [("Chat",msg),("password",password),("name",name)]
  readJSON (JSObject x) | firstKey x == "TextEdit" = EstuaryEdit <$> valFromObj "password" x <*> valFromObj "EstuaryEdit" x <*> valFromObj "code" x
  readJSON (JSObject x) | firstKey x == "TextEdit" = TextEdit <$> valFromObj "password" x <*> valFromObj "TextEdit" x <*> valFromObj "code" x
  readJSON (JSObject x) | firstKey x == "TextEdit" = TextEval <$> valFromObj "password" x <*> valFromObj "TextEval" x <*> valFromObj "code" x
  readJSON (JSObject x) | firstKey x == "Chat" = Chat <$> valFromObj "password" x <*> valFromObj "name" x <*> valFromObj "Chat" x
  readJSON _ = Error "Unable to parse as EstuaryProtocol"

setPassword :: String -> EstuaryProtocol -> EstuaryProtocol
setPassword x (EstuaryEdit _ n c) = EstuaryEdit x n c
setPassword x (TextEdit _ n c) = TextEdit x n c
setPassword x (TextEval _ n c) = TextEval x n c
setPassword x (Chat _ m n) = Chat x m n
