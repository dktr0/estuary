-- TODO: this should be moved/renamed as Estuary.Client.Hint

module Estuary.Types.Hint where

import Data.Text (Text)
import Data.Maybe (mapMaybe)

import Estuary.Types.Tempo
import Estuary.Utility
import Estuary.Types.Definition
import Estuary.Types.TranslatableText
import Estuary.Client.Settings
import Estuary.Types.View

data Hint =
  SampleHint Text |
  LogMessage TranslatableText |
  SetGlobalDelayTime Double |
  SilenceHint |
  ZoneHint Int Definition |
  ChangeSettings (Settings -> Settings) |
  SetLocalView View

justGlobalDelayTime :: [Hint] -> Maybe Double
justGlobalDelayTime = lastOrNothing . mapMaybe f
  where f (SetGlobalDelayTime x) = Just x
        f _ = Nothing
