module Estuary.Types.Sited where

import Text.JSON
import Estuary.Utility (firstKey)
import Data.Maybe

data Sited a b = Sited a b

instance (JSON a, JSON b) => JSON (Sited a b) where
  showJSON (Sited s x) = encJSDict [("at",showJSON s),("x",showJSON x)]
  readJSON (JSObject x) | firstKey x == "at" = Sited <$> valFromObj "at" x <*> valFromObj "x" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as Estuary.Types.Sited" ++ (show x)
  readJSON _ = Error "Unable to parse as Estuary.Types.Sited"

justSited :: Eq a => a -> [Sited a b] -> [b]
justSited x1 = mapMaybe f
  where
    f (Sited x2 y) | x1 == x2 = Just y
    f _ = Nothing
