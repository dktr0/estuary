module Estuary.Types.Definition where

import Text.JSON
import Estuary.Utility (firstKey)
import Estuary.Tidal.Types
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map

data Definition =
  Structure TransformedPattern |
  EvaluableText String |
  LabelText String
  deriving (Eq,Show)

type DefinitionMap = Map.Map Int Definition

instance JSON Definition where
  showJSON (Structure x) = encJSDict [("Structure",x)]
  showJSON (EvaluableText x) = encJSDict [("EvaluableText",x)]
  showJSON (LabelText x) = encJSDict [("LabelText",x)]
  readJSON (JSObject x) | firstKey x == "Structure" = Structure <$> valFromObj "Structure" x
  readJSON (JSObject x) | firstKey x == "EvaluableText" = EvaluableText <$> valFromObj "EvaluableText" x
  readJSON (JSObject x) | firstKey x == "LabelText" = LabelText <$> valFromObj "LabelText" x
  readJSON (JSObject x) | otherwise = Error $ "Unable to parse JSObject as Definition: " ++ (show x)
  readJSON _ = Error "Unable to parse non-JSObject as Definition"

justStructures :: [Definition] -> [TransformedPattern]
justStructures = mapMaybe f
  where f (Structure x) = Just x
        f _ = Nothing

justEvaluableTexts :: [Definition] -> [String]
justEvaluableTexts = mapMaybe f
  where f (EvaluableText x) = Just x
        f _ = Nothing

justLabelTexts :: [Definition] -> [String]
justLabelTexts = mapMaybe f
  where f (LabelText x) = Just x
        f _ = Nothing

