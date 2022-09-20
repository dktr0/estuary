{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Terminal (terminalWidget) where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Control.Monad.IO.Class (liftIO)
import Data.Either
import Data.Maybe
import Data.Map.Strict (fromList)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad
import TextShow
import Sound.MusicW.AudioContext

import Estuary.Protocol.Peer
import Estuary.Types.Definition
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.EnsembleResponse
import Estuary.Types.EnsembleC
import Estuary.Widgets.Reflex
import Estuary.Render.DynamicsMode
import qualified Estuary.Types.Term as Term
import qualified Estuary.Types.Terminal as Terminal
import Estuary.Types.Hint as Hint
import Estuary.Widgets.W
import Estuary.Widgets.EnsembleStatus
import Estuary.Types.TranslatableText
import Estuary.Render.R
import Estuary.Render.MainBus
import qualified Estuary.Render.WebSerial as WebSerial

import Estuary.Types.Language

terminalWidget :: MonadWidget t m => Event t [Response] -> Event t [Hint] -> W t m (Event t Terminal.Command)
terminalWidget deltasDown hints = divClass "terminal code-font" $ mdo
  commands <- divClass "chat" $ mdo
    (inputWidget) <- divClass "terminalHeader code-font primary-color" $ do
      divClass "webSocketButtons" $ term Term.TerminalChat >>= dynText
      let resetText = fmap (const "") terminalInput
      let attrs = constDyn $ fromList [("class","primary-color code-font"),("style","width: 100%")]
      inputWidget' <- divClass "terminalInput" $ textInput $ def & textInputConfig_setValue .~ resetText & textInputConfig_attributes .~ attrs
      return (inputWidget')
    let enterPressed = fmap (const ()) $ ffilter (==13) $ _textInput_keypress inputWidget
    let terminalInput = tag (current $ _textInput_value inputWidget) $ leftmost [enterPressed]
    let parsedInput = fmap Terminal.parseCommand terminalInput
    let commands = fmapMaybe (either (const Nothing) Just) parsedInput
    let errorMsgs = fmap (fmap english) $ fmapMaybe (either (Just . (:[]) . ("Error: " <>) . T.pack . show) (const Nothing)) parsedInput -- [Event t Text]
    let hintMsgs' = fmap hintsToMessages $ mergeWith (++) [hints,fmap pure commandHints] -- Event t [TranslatableText]
    let hintMsgs = ffilter (/= []) hintMsgs' --
    -- parse responses from server in order to display log/chat messages
    let responseMsgs = fmap (\x -> fmap english (Data.Maybe.mapMaybe responseToMessage x)) deltasDown -- [Event t Text]
    let messages = mergeWith (++)  [responseMsgs, errorMsgs, hintMsgs]

    mostRecent <- foldDyn (\a b -> take 12 $ (reverse a) ++ b) [] messages
    divClass "chatMessageContainer" $ simpleList mostRecent $ \v -> do
      v' <- dynTranslatableText v -- W t m (Dynamic t Text)
      divClass "chatMessage code-font primary-color" $ dynText v' -- m()

    pp <- liftIO $ newPeerProtocol
    irc <- renderEnvironment
    commandHints <- performCommands pp irc commands
    return commands

  divClass "ensembleStatus" $ ensembleStatusWidget

  return commands


hintsToMessages :: [Hint] -> [TranslatableText] -- [TranslatableText]
hintsToMessages hs = fmapMaybe hintToMessage hs -- [x ..]

hintToMessage :: Hint -> Maybe TranslatableText
hintToMessage (LogMessage x) = Just x -- translatableText $ Data.Map.fromList [(English, x)]
hintToMessage _ = Nothing

performCommands :: MonadWidget t m => PeerProtocol -> RenderEnvironment -> Event t Terminal.Command -> m (Event t Hint)
performCommands pp rEnv x = do
  y <- performEvent $ fmap (liftIO . doCommands pp rEnv) x
  return $ fmap (LogMessage . english) $ fmapMaybe id y


runCommand :: MonadIO m => PeerProtocol -> RenderEnvironment -> EnsembleC -> Terminal.Command -> m [Hint]

-- change the active view to a local view that is not shared/stored anywhere
runCommand _ _ _ (Terminal.LocalView v) = pure [ LocalView v ]

-- make the current active view a named preset of current ensemble or Estuary itself
runCommand _ _ e (Terminal.PresetView n) = case lookupView n e of
  Just _ -> pure [ PresetView n ]
  Nothing -> pure [ logHint "error: no preset by that name exists" ]

-- take the current local view and publish it with the specified name
runCommand _ _ e (Terminal.PublishView n) = pure [ Request $ WriteView n $ activeView e ]

-- display name of active view if it is standard/published, otherwise report that it is a local view
runCommand _ _ e Terminal.ActiveView = case view e of
  Left _ -> pure [ logHint "(local view)" ]
  Right n -> pure [ logHint n ]

-- display the names of all available standard/published views
runCommand _ _ e Terminal.ListViews = pure [ logHint $ listViews $ ensemble e]

-- display the definition of the current view, regardless of whether standard/published or local
runCommand _ _ e Terminal.DumpView = pure [ logHint $ dumpView $ activeView e ]

-- send a chat message
runCommand _ _ _ (Terminal.Chat msg) = pure [ Request $ SendChat msg ]

-- delay estuary's audio output by the specified time in seconds
runCommand _ _ _ (Terminal.Delay t) = pure [ ChangeSettings $ \s -> s { globalAudioDelay = t } ]

-- send audio input straight to audio output, at specified level in dB (nothing=off)
runCommand _ _ _ (Terminal.MonitorInput maybeDouble) = pure [ ChangeSettings $ \s -> s { monitorInput = maybeDouble } ]

-- delete the current ensemble from the server (with host password)
runCommand _ _ _ (Terminal.DeleteThisEnsemble pwd) = pure [ Request $ DeleteThisEnsemble pwd ]

-- delete the ensemble specified by first argument from the server (with moderator password)
runCommand _ _ _ (Terminal.DeleteEnsemble name pwd) = pure [ Request $ DeleteEnsemble name pwd ]

-- for testing, sets active tempo to one anchored years in the past
runCommand _ _ _ Terminal.AncientTempo = pure [ Request $ WriteTempo t ]
  where t = Tempo { freq = 0.5, time = UTCTime (fromGregorian 2020 01 01) 0, count = 0 }

runCommand _ _ e Terminal.ShowTempo = pure [ logHint $ readableTempo $ tempo $ ensemble e ]

runCommand _ _ e (Terminal.SetCPS x) = do
  t <- liftIO $ changeTempoNow (realToFrac x) (tempo $ ensemble e)
  pure [ Request $ WriteTempo t ]

runCommand _ _ e (Terminal.SetBPM x) = do
  t <- liftIO $ changeTempoNow (realToFrac x / 240) (tempo $ ensemble e)
  pure [ Request $ WriteTempo t ]

runCommand _ _ e (Terminal.InsertSound url bankName n) = do
  let rs = resourceOps $ ensemble e
  let rs' = rs |> InsertResource Audio url (bankName,n)
  pure [ Request $ WriteResourceOps rs' ]

runCommand _ _ e (Terminal.DeleteSound bankName n) = do
  let rs = resourceOps $ ensemble e
  let rs' = rs |> DeleteResource Audio (bankName,n)
  pure [ Request $ WriteResourceOps rs' ]

runCommand _ _ e (Terminal.AppendSound url bankName) = do
  let rs = resourceOps $ ensemble e
  let rs' = rs |> AppendResource Audio url bankName
  pure [ Request $ WriteResourceOps rs' ]

runCommand _ _ e (Terminal.ResList url) = do
  let rs = resourceOps $ ensemble e
  let rs' = rs |> ResourceListURL url
  pure [ Request $ WriteResourceOps rs' ]

runCommand _ _ _ Terminal.ClearResources = pure [ Request $ WriteResourceOps Seq.empty ]

runCommand _ _ _ Terminal.DefaultResources = pure [ Request $ WriteResourceOps defaultResourceOps ]

runCommand _ _ e Terminal.ShowResources = pure [ logHint $ showResourceOps $ resourceOps $ ensemble e ]

runCommand _ _ _ Terminal.ResetZones = pure [ Request ResetZones ]

runCommand _ _ _ Terminal.ResetViews = pure [ Request ResetViews ]

runCommand _ _ _ Terminal.ResetTempo = do
  t <- liftIO getCurrentTime
  pure [ Request $ WriteTempo $ Tempo { freq = 0.5, time = t, count = 0 } ]

runCommand _ _ _ Terminal.Reset = do
  t <- liftIO getCurrentTime
  pure [ Request $ Reset $ Tempo { freq = 0.5, time = t, count = 0 } ]

CONTINUE HERE

  SetCC Int Double | -- set a MIDI continuous-controller value (range of Double is 0-1)
  ShowCC Int | -- show a MIDI continuous-controller value in the terminal
  MaxAudioOutputs | -- query max number of audio output channels according to browser
  SetAudioOutputs Int | -- attempt to set a specific number of audio output channels
  AudioOutputs | -- query current number of output audio channels
  ListSerialPorts | -- query available WebSerial ports
  SetSerialPort Int | -- select a WebSerial port by index, and activate WebSerial output
  NoSerialPort -- disactivate WebSerial output

doCommands _ irc (Terminal.SetCC n v) = setCC n v irc >> return Nothing
doCommands _ irc (Terminal.ShowCC n) = do
  x <- getCC n irc -- :: Maybe Double
  return $ Just $ case x of
    Just x' -> "CC" <> showt n <> " = " <> showt x'
    Nothing -> "CC" <> showt n <> " not set"
doCommands _ _ Terminal.MaxAudioOutputs = liftAudioIO $ do
  n <- maxChannelCount
  return $ Just $ "maxAudioOutputs = " <> showt n
doCommands _ _ Terminal.AudioOutputs = liftAudioIO $ do
  n <- channelCount
  return $ Just $ "audioOutputs = " <> showt n
doCommands _ irc (Terminal.SetAudioOutputs n) = do
  setAudioOutputs (webDirt irc) (mainBus irc) n
  n' <- liftAudioIO $ channelCount
  return $ Just $ "audioOutputs = " <> showt n'
doCommands _ rEnv Terminal.ListSerialPorts = do
  portMap <- WebSerial.listPorts (webSerial rEnv)
  pure $ Just $ T.pack $ show portMap
doCommands _ rEnv (Terminal.SetSerialPort n) = WebSerial.setActivePort (webSerial rEnv) n >> pure (Just "serial port set")
doCommands _ rEnv Terminal.NoSerialPort = WebSerial.setNoActivePort (webSerial rEnv) >> pure (Just "serial port disactivated")
doCommands _ _ _ = return Nothing



commandToHint :: EnsembleC -> Terminal.Command -> Maybe Hint
commandToHint _ (Terminal.LocalView _) = Just $ LogMessage $ (Map.fromList [(English,  "local view changed"), (Español, "La vista local ha cambiado")])
commandToHint _ (Terminal.PresetView x) = Just $ LogMessage $ (Map.fromList [(English,  "preset view " <> x <> " selected"), (Español, "vista predeterminada " <> x <> " seleccionada")])
commandToHint _ (Terminal.PublishView x) = Just $ LogMessage $ (Map.fromList [(English, "active view published as " <> x), (Español, "vista activa publicada como " <> x)])
commandToHint es (Terminal.ActiveView) = Just $ LogMessage $ (english $ nameOfActiveView es)
commandToHint es (Terminal.ListViews) = Just $ LogMessage $  (english $ showt $ listViews $ ensemble es)
commandToHint es (Terminal.DumpView) = Just $ LogMessage $  (english $ dumpView (activeView es))
commandToHint _ (Terminal.Delay t) = Just $ ChangeSettings (\s -> s { globalAudioDelay = t } )
commandToHint _ Terminal.ResetZones = Just $ LogMessage  (Map.fromList [(English, "zones reset"), (Español, "zonas reiniciadas")])
commandToHint _ Terminal.ResetViews = Just $ LogMessage  (Map.fromList [(English, "views reset"), (Español, "vistas reiniciadas")])
commandToHint _ Terminal.ResetTempo = Just $ LogMessage  (Map.fromList [(English, "tempo reset"), (Español, "tempo reiniciado")])
commandToHint _ Terminal.Reset = Just $ LogMessage (Map.fromList [(English, "(full) reset"), (Español, "reinicio (completo)")])
commandToHint _ (Terminal.LocalView v) = Just $ SetLocalView v
commandToHint _ _ = Nothing

-- WORK IN PROGRESS: this definition should cease to exist, all patterns migrate above to commandToHint
commandToStateChange :: Terminal.Command -> EnsembleC -> EnsembleC
commandToStateChange (Terminal.PresetView t) es = selectPresetView t es
commandToStateChange (Terminal.PublishView t) es = replaceStandardView t (activeView es) es
commandToStateChange _ es = es
