module Estuary.Types.EditOrEval where

import Text.JSON
import Estuary.Utility (firstKey)

data EditOrEval a = Edit a | Evaluate a deriving (Show,Eq)

instance JSON a => JSON (EditOrEval a) where
  showJSON (Edit a) = encJSDict [("Edit",a)]
  showJSON (Evaluate a) = encJSDict [("Evaluate",a)]
  readJSON (JSObject x) | firstKey x == "Edit" = Edit <$> valFromObj "Edit" x
  readJSON (JSObject x) | firstKey x == "Evaluate" = Evaluate <$> valFromObj "Evaluate" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as EditOrEval: " ++ (show x)
  readJSON _ = Error "Unable to parse non-JSObject as EditOrEval"
