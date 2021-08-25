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

data TimerDownState = 
  Holding Int |  -- target     
  Falling Int UTCTime -- target and start time
  deriving (Eq,Show,Generic)

data TimerUpState =
  Cleared |
  Running UTCTime |
  Stopped NominalDiffTime
  deriving (Eq, Show, Generic)

instance ToJSON TimerUpState where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TimerUpState

instance ToJSON TimerDownState where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TimerDownState

data Definition =
  TextProgram (Live TextProgram) |
  Sequence Sequence |
  TidalStructure TransformedPattern |
  LabelText Text |
  Roulette Roulette |
  CountDown TimerDownState |
  SandClock TimerDownState |
  StopWatch TimerUpState
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
definitionForRendering (CountDown x) = CountDown x
definitionForRendering (SandClock x) = SandClock x
definitionForRendering (StopWatch x) = StopWatch x

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

maybeTimerUpState:: Definition -> Maybe TimerUpState
maybeTimerUpState (StopWatch x) = Just x
maybeTimerUpState _ = Nothing

maybeTimerDownState:: Definition -> Maybe TimerDownState
maybeTimerDownState (CountDown x) = Just x
maybeTimerDownState (SandClock x) = Just x
maybeTimerDownState _ = Nothing
