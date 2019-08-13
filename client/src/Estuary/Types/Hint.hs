module Estuary.Types.Hint where

import Data.Text (Text)

import Estuary.Types.Tempo

data Hint =
  SampleHint Text |
  LogMessage Text
  deriving (Eq,Show)
