module Estuary.Types.Hint where

import Data.Text (Text)
import Data.Maybe (mapMaybe)

import Estuary.Types.Tempo
import Estuary.Utility

data Hint =
  SampleHint Text |
  LogMessage Text |
  SetGlobalDelayTime Double
  deriving (Eq,Show)

justGlobalDelayTime :: [Hint] -> Maybe Double
justGlobalDelayTime = lastOrNothing . mapMaybe f
  where f (SetGlobalDelayTime x) = Just x
        f _ = Nothing
