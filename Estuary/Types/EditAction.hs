module Estuary.Types.EditAction where

import Text.JSON

data EditAction a = EditAction a | EvalAction

instance JSON a => JSON (EditAction a) where
  showJSON (EditAction x) = encJSDict [("EditAction",x)]
  showJSON (EvalAction) = showJSON "EvalAction"
  readJSON (JSObject x) | (fst . head . fromJSObject) x == "EditAction" = EditAction <$> valFromObj "EditAction" x
  readJSON (JSString x) | fromJSString x == "EvalAction" = Ok EvalAction
  readJSON _ = Error "Can't parse as EditAction"
