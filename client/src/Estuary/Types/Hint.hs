-- TODO: this should be moved/renamed as Estuary.Client.Hint

module Estuary.Types.Hint where

import Data.Text (Text)
import Data.Maybe (mapMaybe)

import Estuary.Types.Tempo
import Estuary.Utility
import Estuary.Types.Definition
import Estuary.Types.TranslatableText
import Estuary.Client.Settings

data Hint =
  SampleHint Text |
  LogMessage TranslatableText |
  SetGlobalDelayTime Double |
  SilenceHint |
  ZoneHint Int Definition |
  ToggleTerminal | -- can be implemented as a Setting (ie. with ChangeSettings)
  ToggleSidebar | -- can be implemented as a Setting (ie. with ChangeSettings)
  ToggleStats | -- can be implemented as a Setting (ie. with ChangeSettings)
  ToggleHeader | -- can be implemented as a Setting (ie. with ChangeSettings)
  ChangeSettings (Settings -> Settings)

justGlobalDelayTime :: [Hint] -> Maybe Double
justGlobalDelayTime = lastOrNothing . mapMaybe f
  where f (SetGlobalDelayTime x) = Just x
        f _ = Nothing

justToggleStats :: [Hint] -> Maybe ()
justToggleStats = lastOrNothing . mapMaybe f
  where f ToggleStats = Just ()
        f _ = Nothing

justToggleSidebar :: [Hint] -> Maybe ()
justToggleSidebar = lastOrNothing . mapMaybe f
  where f ToggleSidebar = Just ()
        f _ = Nothing

justToggleTerminal :: [Hint] -> Maybe ()
justToggleTerminal = lastOrNothing . mapMaybe f
  where f ToggleTerminal = Just ()
        f _ = Nothing
