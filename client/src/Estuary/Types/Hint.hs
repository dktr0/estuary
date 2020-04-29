module Estuary.Types.Hint where

import Data.Text (Text)
import Data.Maybe (mapMaybe)

import Estuary.Types.Tempo
import Estuary.Utility
import Estuary.Types.Definition

data Hint =
  SampleHint Text |
  LogMessage Text |
  SetGlobalDelayTime Double |
  SilenceHint |
  ZoneHint Int Definition |
  ToggleTerminal |
  ToggleStats |
  ToggleHeader
  deriving (Eq,Show)

justGlobalDelayTime :: [Hint] -> Maybe Double
justGlobalDelayTime = lastOrNothing . mapMaybe f
  where f (SetGlobalDelayTime x) = Just x
        f _ = Nothing
