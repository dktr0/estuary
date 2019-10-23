{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.Definition where

import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import GHC.Generics
import Data.Aeson

import Estuary.Tidal.Types
import Estuary.Types.Live
import Estuary.Types.TextNotation

type TextProgram = Live (TextNotation,Text)

type Sequence = M.Map Int (Text,[Bool])

data Definition =
  Structure TransformedPattern | -- *** this should be renamed to TidalStructure
  TextProgram TextProgram |
  Sequence Sequence |
  LabelText Text
  deriving (Eq,Show,Generic)

instance ToJSON Definition where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Definition

type DefinitionMap = IntMap.IntMap Definition

emptyDefinitionMap :: DefinitionMap
emptyDefinitionMap = IntMap.empty

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

maybeTextProgram :: Definition -> Maybe TextProgram
maybeTextProgram (TextProgram x) = Just x
maybeTextProgram _ = Nothing

justTextPrograms :: [Definition] -> [TextProgram]
justTextPrograms = mapMaybe maybeTextProgram

maybeSequence :: Definition -> Maybe Sequence
maybeSequence (Sequence x) = Just x
maybeSequence _ = Nothing

justSequences :: [Definition] -> [Sequence]
justSequences = mapMaybe maybeSequence

maybeLabelText :: Definition -> Maybe Text
maybeLabelText (LabelText x) = Just x
maybeLabelText _ = Nothing

justLabelTexts :: [Definition] -> [Text]
justLabelTexts = mapMaybe maybeLabelText
