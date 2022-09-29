-- TODO: this should be moved/renamed as Estuary.Client.Hint

module Estuary.Types.Hint where

import Data.Text (Text,pack)
import Data.Maybe (mapMaybe)
import TextShow

import Estuary.Types.Tempo
import Estuary.Utility
import Estuary.Types.Definition
import Estuary.Types.TranslatableText
import Estuary.Client.Settings
import Estuary.Types.View
import Estuary.Types.Request

data Hint =
  PreloadAudioBank Text | -- TODO: supposed to cause preload of every sample in that bank, hasn't been reimplemented since new Resources system though (currently a no-op)
  LogMessage TranslatableText | -- message is printed in Estuary's terminal (response: Estuary.Widgets.Terminal)
  ChangeSettings (Settings -> Settings) | -- change Settings of local UI and rendering (response: settingsForWidgets in Estuary.Widgets.Estuary)
  Request Request | -- directly issue a Request
  LocalView View |
  PresetView Text |
  SetCC Int Double -- TODO: above W tree, SetCC hint needs to become IO operation with RenderEnvironment

logText :: Text -> Hint
logText = LogMessage . english

logTextShow :: TextShow a => a -> Hint
logTextShow = LogMessage . english . showt

logShow :: Show a => a -> Hint
logShow = LogMessage . english . pack . show

hintsToRequests :: [Hint] -> [Request]
hintsToRequests = mapMaybe f
  where f (Request r) = Just r
        f _ = Nothing
