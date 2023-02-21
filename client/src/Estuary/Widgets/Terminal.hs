{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Terminal (terminalWidget) where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Control.Monad.IO.Class (liftIO,MonadIO)
import Data.Either
import Data.Maybe
import Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Reader
import TextShow
import Sound.MusicW.AudioContext
import Data.Sequence as Seq
import Data.Time

import Estuary.Types.Definition
import Estuary.Types.Request as Request
import Estuary.Types.Response as Response
import Estuary.Types.EnsembleC as EnsembleC
import Estuary.Types.Ensemble as Ensemble
import Estuary.Widgets.Reflex
import Estuary.Render.DynamicsMode
import qualified Estuary.Types.Term as Term
import qualified Estuary.Types.Terminal as Terminal
import Estuary.Types.Hint as Hint
import Estuary.Widgets.W as W
import Estuary.Widgets.EnsembleStatus
import Estuary.Types.TranslatableText
import Estuary.Render.R
import Estuary.Render.MainBus
import Estuary.Client.Settings as Settings
import qualified Estuary.Render.WebSerial as WebSerial
import Estuary.Types.Tempo
import Estuary.Types.Language
import Estuary.Types.View.Parser
import Estuary.Types.ResourceOp
import Estuary.Types.ResourceType
import Estuary.Types.LogEntry

terminalWidget :: MonadWidget t m => W t m ()
terminalWidget = divClass "terminal code-font" $ mdo
  divClass "chat" $ mdo

    -- take input
    inputWidget <- divClass "terminalHeader code-font primary-color" $ do
      divClass "webSocketButtons" $ term Term.TerminalChat >>= dynText
      let attrs = constDyn $ Map.fromList [("class","primary-color code-font"),("style","width: 100%")]
      divClass "terminalInput" $ textInput $ def & textInputConfig_setValue .~ ("" <$ terminalInput) & textInputConfig_attributes .~ attrs
    let enterPressed = fmap (const ()) $ ffilter (==13) $ _textInput_keypress inputWidget
    let terminalInput = tag (current $ _textInput_value inputWidget) $ leftmost [enterPressed]
    let maybeCommand = fmap Terminal.parseCommand terminalInput
    let command = fmapMaybe (either (const Nothing) Just) maybeCommand
    let errorMsg = fmapMaybe (either (Just . ("Error: " <>) . T.pack . show) (const Nothing)) maybeCommand
    hint $ fmap logText errorMsg

    -- display log
    lang <- W.language
    logEntries <- W.log
    let mostRecent = fmap (Prelude.take 50) logEntries
    let entriesAsText = showtLogEntries <$> lang <*> mostRecent
    divClass "chatMessageContainer" $ simpleList entriesAsText $ \v -> do
      divClass "chatMessage code-font primary-color" $ dynText v


    rEnv <- renderEnvironment
    ensC <- lift $ asks _ensembleC
    (performEvent $ attachWith (runCommand rEnv) (current ensC) command) >>= hints

  divClass "ensembleStatus" $ ensembleStatusWidget


showtLogEntries :: Language -> [LogEntry] -> [Text]
showtLogEntries l xs = fmap (showtLogEntry l) xs

runCommand :: MonadIO m => RenderEnvironment -> EnsembleC -> Terminal.Command -> m [Hint]

-- change the active view to a local view that is not shared/stored anywhere
runCommand _ _ (Terminal.LocalView v) =
  pure [
    LocalView v,
    LocalLog $ (Map.fromList [
      (English,  "local view changed"),
      (Español, "La vista local ha cambiado")
      ])
  ]

-- make the current active view a named preset of current ensemble or Estuary itself
runCommand _ e (Terminal.PresetView x) = case lookupView x (ensemble e) of
  Just _ -> pure [
    PresetView x,
    LocalLog $ (Map.fromList [
      (English, "preset view " <> x <> " selected"),
      (Español, "vista predeterminada " <> x <> " seleccionada")
      ])
    ]
  Nothing -> pure [ logText "error: no preset by that name exists" ]

-- take the current local view and publish it with the specified name
runCommand _ e (Terminal.PublishView x) = pure [
  Request $ Request.WriteView x $ EnsembleC.activeView e,
  LocalLog $ (Map.fromList [
    (English, "active view published as " <> x),
    (Español, "vista activa publicada como " <> x)
    ])
  ]

-- display name of active view if it is standard/published, otherwise report that it is a local view
runCommand _ e Terminal.ActiveView = pure [ logText $ EnsembleC.nameOfActiveView e ]

-- display the names of all available standard/published views
runCommand _ e Terminal.ListViews = pure [ logTextShow $ listViews $ ensemble e]

-- display the definition of the current view, regardless of whether standard/published or local
runCommand _ e Terminal.DumpView = pure [ logText $ dumpView $ EnsembleC.activeView e ]

-- send a chat message
runCommand _ _ (Terminal.Chat msg) = pure [ Request $ SendChat msg ]

-- delay estuary's audio output by the specified time in seconds
runCommand _ _ (Terminal.Delay t) = pure [ ChangeSettings $ \s -> s { Settings.globalAudioDelay = t } ]

-- send audio input straight to audio output, at specified level in dB (nothing=off)
runCommand _ _ (Terminal.MonitorInput maybeDouble) = pure [ ChangeSettings $ \s -> s { monitorInput = maybeDouble } ]

-- delete the current ensemble from the server (with host password)
runCommand _ _ (Terminal.DeleteThisEnsemble pwd) = pure [ Request $ DeleteThisEnsemble pwd ]

-- delete the ensemble specified by first argument from the server (with moderator password)
runCommand _ _ (Terminal.DeleteEnsemble name pwd) = pure [ Request $ DeleteEnsemble name pwd ]

-- for testing, sets active tempo to one anchored years in the past
runCommand _ _ Terminal.AncientTempo = pure [ Request $ Request.WriteTempo t ]
  where t = Tempo { freq = 0.5, time = UTCTime (fromGregorian 2020 01 01) 0, Estuary.Types.Tempo.count = 0 }

runCommand _ e Terminal.ShowTempo = pure [ logText $ readableTempo $ Ensemble.tempo $ ensemble e ]

runCommand _ e (Terminal.SetCPS x) = do
  t <- liftIO $ changeTempoNow (realToFrac x) (Ensemble.tempo $ ensemble e)
  pure [ Request $ Request.WriteTempo t ]

runCommand _ e (Terminal.SetBPM x) = do
  t <- liftIO $ changeTempoNow (realToFrac x / 240) (Ensemble.tempo $ ensemble e)
  pure [ Request $ Request.WriteTempo t ]

runCommand _ e (Terminal.InsertSound url bankName n) = do
  let rs = Ensemble.resourceOps $ ensemble e
  let rs' = rs |> InsertResource Audio url (bankName,n)
  pure [ Request $ Request.WriteResourceOps rs' ]

runCommand _ e (Terminal.DeleteSound bankName n) = do
  let rs = Ensemble.resourceOps $ ensemble e
  let rs' = rs |> DeleteResource Audio (bankName,n)
  pure [ Request $ Request.WriteResourceOps rs' ]

runCommand _ e (Terminal.AppendSound url bankName) = do
  let rs = Ensemble.resourceOps $ ensemble e
  let rs' = rs |> AppendResource Audio url bankName
  pure [ Request $ Request.WriteResourceOps rs' ]

runCommand _ e (Terminal.ResList url) = do
  let rs = Ensemble.resourceOps $ ensemble e
  let rs' = rs |> ResourceListURL url
  pure [ Request $ Request.WriteResourceOps rs' ]

runCommand _ _ Terminal.ClearResources = pure [ Request $ Request.WriteResourceOps Seq.empty ]

runCommand _ _ Terminal.DefaultResources = pure [ Request $ Request.WriteResourceOps defaultResourceOps ]

runCommand _ e Terminal.ShowResources = pure [ logText $ showResourceOps $ Ensemble.resourceOps $ ensemble e ]

runCommand _ _ Terminal.ResetZones = pure [
  Request Request.ResetZones,
  LocalLog (Map.fromList [
    (English, "zones reset"),
    (Español, "zonas reiniciadas")
    ])
  ]

runCommand _ _ Terminal.ResetViews = pure [
  Request Request.ResetViews,
  LocalLog (Map.fromList [
    (English, "views reset"),
    (Español, "vistas reiniciadas")
    ])
  ]

runCommand _ _ Terminal.ResetTempo = do
  t <- liftIO getCurrentTime
  pure [
    Request $ Request.WriteTempo $ Tempo { freq = 0.5, time = t, Estuary.Types.Tempo.count = 0 },
    LocalLog (Map.fromList [
      (English, "tempo reset"),
      (Español, "tempo reiniciado")
      ])
    ]

runCommand _ _ Terminal.Reset = do
  t <- liftIO getCurrentTime
  pure [
    Request $ Request.Reset $ Tempo { freq = 0.5, time = t, Estuary.Types.Tempo.count = 0 },
    LocalLog (Map.fromList [
      (English, "(full) reset"),
      (Español, "reinicio (completo)")
      ])
    ]

-- set a MIDI continuous-controller value (range of Double is 0-1)
runCommand _ _ (Terminal.SetCC n v) = pure [ SetCC n v ]

-- show a MIDI continuous-controller value in the terminal
runCommand rEnv _ (Terminal.ShowCC n) = do
  x <- liftIO $ getCC n rEnv -- :: Maybe Double
  pure [ logText $ case x of
    Just x' -> "CC" <> showt n <> " = " <> showt x'
    Nothing -> "CC" <> showt n <> " not set"
    ]

-- query max number of audio output channels according to browser
runCommand _ _ Terminal.MaxAudioOutputs = do
  n <- liftAudioIO maxChannelCount
  pure [ logText $ "maxAudioOutputs = " <> showt n ]

-- attempt to set a specific number of audio output channels
runCommand rEnv _ (Terminal.SetAudioOutputs n) = do
  liftIO $ setAudioOutputs (webDirt rEnv) (mainBus rEnv) n
  n' <- liftAudioIO channelCount
  pure [ logText $ "audioOutputs = " <> showt n' ]

-- query current number of output audio channels
runCommand _ _ Terminal.AudioOutputs = do
  n <- liftAudioIO channelCount
  pure [ logText $ "audioOutputs = " <> showt n ]

-- query available WebSerial ports
runCommand rEnv _ Terminal.ListSerialPorts = do
  portMap <- liftIO $ WebSerial.listPorts (webSerial rEnv)
  pure [ logShow portMap ]

-- select a WebSerial port by index, and activate WebSerial output
runCommand rEnv _ (Terminal.SetSerialPort n) = do
  liftIO $ WebSerial.setActivePort (webSerial rEnv) n
  pure [ logText "serial port set" ]

-- disactivate WebSerial output
runCommand rEnv _ Terminal.NoSerialPort = do
  liftIO $ WebSerial.setNoActivePort (webSerial rEnv)
  pure [ logText "serial port disactivated" ]
