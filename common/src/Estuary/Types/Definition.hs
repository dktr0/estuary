{-# LANGUAGE DeriveGeneric #-}

module Estuary.Types.Definition where

import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import GHC.Generics
import Data.Aeson
import Data.Time

import Estuary.Tidal.Types
import Estuary.Types.Live
import Estuary.Types.TextNotation

type TextProgram = (TextNotation,Text,UTCTime)

type Sequence = M.Map Int (Text,[Bool])
type Roulette = [Text]

data Definition =
  TextProgram (Live TextProgram) |
  Sequence Sequence |
  TidalStructure TransformedPattern |
  LabelText Text |
  Roulette Roulette
  deriving (Eq,Show,Generic)

instance ToJSON Definition where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Definition

type DefinitionMap = IntMap.IntMap Definition

emptyDefinitionMap :: DefinitionMap
emptyDefinitionMap = IntMap.empty

definitionForRendering :: Definition -> Definition
definitionForRendering (TextProgram x) = TextProgram (Live (forRendering x) L4)
definitionForRendering (Sequence x) = Sequence x
definitionForRendering (TidalStructure x) = TidalStructure x
definitionForRendering (LabelText x) = LabelText x
definitionForRendering (Roulette x) = Roulette x

-- note: assumes both definitions are results of definitionForRendering (above)
definitionChanged :: Definition -> Definition -> Bool
definitionChanged (TextProgram x) (TextProgram y) = textProgramChanged x y
definitionChanged (Sequence x) (Sequence y) = x /= y
definitionChanged (TidalStructure x) (TidalStructure y) = True -- structure edits likely to be changes
definitionChanged _ _ = False

definitionNotationChanged :: Definition -> Definition -> Bool
definitionNotationChanged (TextProgram x) (TextProgram y) = textProgramNotationChanged x y
definitionNotationChanged (Sequence x) (Sequence y) = False
definitionNotationChanged (TidalStructure x) (TidalStructure y) = False
definitionNotationChanged _ _ = True

-- just looks at evaluation time to determine if a text program has changed for rendering purposes
textProgramChanged :: TextProgram -> TextProgram -> Bool
textProgramChanged (Live (_,_,x) L4) (Live (_,_,y) L4) = x /= y
textProgramChanged _ _ = error "textProgramChanged must have been called without definitionForRendering"

textProgramNotationChanged :: TextProgram -> TextProgram -> Bool
textProgramNotationChanged (Live (x,_,_) L4) (Live (y,_,_) L4) = x /= y
textProgramNotationChanged _ _ = error "textProgramNotationChanged must have been called without definitionForRendering"

maybeTidalStructure :: Definition -> Maybe TransformedPattern
maybeTidalStructure (TidalStructure x) = Just x
maybeTidalStructure _ = Nothing

justTidalStructures :: [Definition] -> [TransformedPattern]
justTidalStructures = mapMaybe maybeTidalStructure

maybeTextProgram :: Definition -> Maybe (Live TextProgram)
maybeTextProgram (TextProgram x) = Just x
maybeTextProgram _ = Nothing

justTextPrograms :: [Definition] -> [Live TextProgram]
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

maybeRoulette :: Definition -> Maybe Roulette
maybeRoulette (Roulette x) = Just x
maybeRoulette _ = Nothing

justRoulettes :: [Definition] -> [Roulette]
justRoulettes = mapMaybe maybeRoulette
