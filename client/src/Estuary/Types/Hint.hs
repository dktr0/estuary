module Estuary.Types.Hint where

import Sound.Tidal.Tempo

data Hint =
  SampleHint String |
  TempoHint Tempo
  deriving (Eq,Show)

instance Eq Tempo where
  (Tempo at' beat' cps' paused' clockLatency') == (Tempo at'' beat'' cps'' paused'' clockLatency'') =
    (at'==at'') && (beat'==beat'') && (cps'==cps'') && (paused'==paused'') && (clockLatency'==clockLatency'')

maybeTempoHint :: Hint -> Maybe Tempo
maybeTempoHint (TempoHint x) = Just x
maybeTempoHint _ = Nothing
