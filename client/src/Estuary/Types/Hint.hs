module Estuary.Types.Hint where

import Estuary.Types.Tempo

data Hint =
  SampleHint String |
  LogMessage String |
  TempoHint Tempo -- ??? what is this for ???
  deriving (Eq,Show)

maybeTempoHint :: Hint -> Maybe Tempo
maybeTempoHint (TempoHint x) = Just x
maybeTempoHint _ = Nothing
