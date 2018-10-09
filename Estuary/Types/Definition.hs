{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.Definition where

import Text.JSON
import Text.JSON.Generic
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map

import Estuary.Utility (firstKey)
import Estuary.Tidal.Types
import Estuary.Types.Live
import Estuary.Types.TextNotation

data Definition =
  Structure TransformedPattern | -- *** this should be renamed to TidalStructure
  TextProgam (Live (TextNotation,String)) |
  EvaluableText String |
  LabelText String
  deriving (Eq,Show,Data,Typeable)

type DefinitionMap = Map.Map Int Definition

instance JSON Definition where
  showJSON = toJSON
  readJSON = fromJSON

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
