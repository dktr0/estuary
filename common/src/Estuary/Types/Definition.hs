{-# LANGUAGE DeriveDataTypeable #-}

module Estuary.Types.Definition where

import Text.JSON
import Text.JSON.Generic
import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)

import Estuary.Tidal.Types
import Estuary.Types.Live
import Estuary.Types.TextNotation

data Definition =
  Structure TransformedPattern | -- *** this should be renamed to TidalStructure
  TextProgram (Live (TextNotation,Text)) |
  Sequence (M.Map Int (Text,[Bool])) |
  LabelText Text
  deriving (Eq,Show,Data,Typeable)

type DefinitionMap = IntMap.IntMap Definition

emptyDefinitionMap :: DefinitionMap
emptyDefinitionMap = IntMap.empty

instance JSON Definition where
  showJSON = toJSON
  readJSON = fromJSON

definitionForRendering :: Definition -> Definition
definitionForRendering (Structure x) = Structure x
definitionForRendering (TextProgram x) = TextProgram (Live (forRendering x) L4)
definitionForRendering (LabelText x) = LabelText x
definitionForRendering (Sequence x) = Sequence x

justStructures :: [Definition] -> [TransformedPattern]
justStructures = mapMaybe f
  where f (Structure x) = Just x
        f _ = Nothing

justTextPrograms :: [Definition] -> [Live (TextNotation,Text)]
justTextPrograms = mapMaybe f
  where f (TextProgram x) = Just x
        f _ = Nothing

justSequences :: [Definition] -> [M.Map Int (Text,[Bool])]
justSequences = mapMaybe f
  where f (Sequence x) = Just x
        f _ = Nothing

justLabelTexts :: [Definition] -> [Text]
justLabelTexts = mapMaybe f
  where f (LabelText x) = Just x
        f _ = Nothing
