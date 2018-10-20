{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.Definition where

import Text.JSON
import Text.JSON.Generic
import Data.Maybe (mapMaybe)
import qualified Data.IntMap.Strict as IntMap

import Estuary.Tidal.Types
import Estuary.Types.Live
import Estuary.Types.TextNotation

data Definition =
  Structure TransformedPattern | -- *** this should be renamed to TidalStructure
  TextProgram (Live (TextNotation,String)) |
  EvaluableText String |
  LabelText String
  deriving (Eq,Show,Data,Typeable)

type DefinitionMap = IntMap.IntMap Definition

instance JSON Definition where
  showJSON = toJSON
  readJSON = fromJSON

definitionForRendering :: Definition -> Definition
definitionForRendering (Structure x) = Structure x
definitionForRendering (TextProgram x) = TextProgram (Live (forRendering x) L4)
definitionForRendering (EvaluableText x) = EvaluableText x
definitionForRendering (LabelText x) = LabelText x

justStructures :: [Definition] -> [TransformedPattern]
justStructures = mapMaybe f
  where f (Structure x) = Just x
        f _ = Nothing

justTextPrograms :: [Definition] -> [Live (TextNotation,String)]
justTextPrograms = mapMaybe f
  where f (TextProgram x) = Just x
        f _ = Nothing

justEvaluableTexts :: [Definition] -> [String]
justEvaluableTexts = mapMaybe f
  where f (EvaluableText x) = Just x
        f _ = Nothing

justLabelTexts :: [Definition] -> [String]
justLabelTexts = mapMaybe f
  where f (LabelText x) = Just x
        f _ = Nothing
