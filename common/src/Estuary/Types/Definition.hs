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
type Counter = Either (Maybe NominalDiffTime) UTCTime


-- un reloj que either (tiempo acumulado) o NOW + Target
data Clock = TimerUp (Either (Maybe NominalDiffTime) UTCTime) | 
             TimerDown (Either (Maybe NominalDiffTime) (UTCTime)) NominalDiffTime
  deriving (Eq,Show,Generic)

instance ToJSON Clock where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Clock

data Definition =
  TextProgram (Live TextProgram) |
  Sequence Sequence |
  TidalStructure TransformedPattern |
  LabelText Text |
  Roulette Roulette |
  CountDown Clock |
  StopWatch' Clock |
  StopWatch Counter
 -- StopWatch Counter |
 -- CountDown Counter     -- this needs to be sorted at some point !!!!!!
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

maybeCounter :: Definition -> Maybe Counter
maybeCounter (StopWatch x) = Just x

maybeClock :: Definition -> Maybe Clock
maybeClock (CountDown  x) = Just x
maybeClock (StopWatch' x) = Just x
maybeClock _ = Nothing
