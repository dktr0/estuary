module Estuary.Types.Hint where

import Estuary.Types.Tempo

data Hint =
  SampleHint String |
  TempoHint Tempo
  deriving (Eq)

maybeTempoHint :: Hint -> Maybe Tempo
maybeTempoHint (TempoHint x) = Just x
maybeTempoHint _ = Nothing
