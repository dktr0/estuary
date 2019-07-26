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

maybeStructure :: Definition -> Maybe TransformedPattern
maybeStructure (Structure x) = Just x
maybeStructure _ = Nothing

justStructures :: [Definition] -> [TransformedPattern]
justStructures = mapMaybe maybeStructure

maybeTextProgram :: Definition -> Maybe (Live (TextNotation,Text))
maybeTextProgram (TextProgram x) = Just x
maybeTextProgram _ = Nothing

justTextPrograms :: [Definition] -> [Live (TextNotation,Text)]
justTextPrograms = mapMaybe maybeTextProgram

maybeSequence :: Definition -> Maybe (M.Map Int (Text,[Bool]))
maybeSequence (Sequence x) = Just x
maybeSequence _ = Nothing

justSequences :: [Definition] -> [M.Map Int (Text,[Bool])]
justSequences = mapMaybe maybeSequence

maybeLabelText :: Definition -> Maybe Text
maybeLabelText (LabelText x) = Just x
maybeLabelText _ = Nothing

justLabelTexts :: [Definition] -> [Text]
justLabelTexts = mapMaybe maybeLabelText
