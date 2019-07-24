module Estuary.Types.Hint where

import Data.Text (Text)

import Estuary.Types.Tempo

data Hint =
  SampleHint Text |
  LogMessage Text |
  TempoHint Tempo -- ??? what is this for ???
  deriving (Eq,Show)

maybeTempoHint :: Hint -> Maybe Tempo
maybeTempoHint (TempoHint x) = Just x
maybeTempoHint _ = Nothing
