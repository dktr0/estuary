module Estuary.Protocol where

import Text.JSON
import Estuary.Tidal.Types

data EstuaryProtocol =
  TextEdit String Int String |
  TextEval String Int String |
  EstuaryEdit String Int TransformedPattern
  deriving (Show)

instance JSON EstuaryProtocol where
  showJSON (TextEdit password n code) = encJSDict [("TextEdit",showJSON n),("password":password),("code":code)]
  showJSON (TextEval password n code) = encJSDict [("TextEval",showJSON n),("password":password),("code":code)]
  showJSON (EstuaryEdit password n code) = encJSDict [("EstuaryEdit",showJSON n),("password":password),("code":showJSON code)]
  readJSON (JSObject x) | firstKey x == "TextEdit" = TextEdit <$> valFromObj "password" x <$> valFromObject "TextEdit" x <$> valFromObject "code" x
  readJSON (JSObject x) | firstKey x == "TextEdit" = TextEval <$> valFromObj "password" x <$> valFromObject "TextEval" x <$> valFromObject "code" x
  readJSON (JSObject x) | firstKey x == "TextEdit" = EstuaryEdit <$> valFromObj "password" x <$> valFromObject "EstuaryEdit" x <$> valFromObject "code" x
