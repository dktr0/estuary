{-# LANGUAGE RecursiveDo, JavaScriptFFI, OverloadedStrings, FlexibleContexts #-}

module Estuary.Widgets.Estuary where

import Control.Monad (liftM)

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response,append,Error)
import Reflex.Dom.Old
import Reflex.Dynamic
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Fix (MonadFix)
import Data.Time
import Data.Map
import Data.Maybe
import Text.Read
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar
import GHCJS.Types
import GHCJS.DOM.Types hiding (Event,Request,Response,Text)
import GHCJS.Marshal.Pure
import GHCJS.DOM.EventM
import Data.Functor (void)
import Data.Text (Text)
import Data.Bool
import qualified Data.Text as T
import qualified Data.Text.IO as T
import TextShow
import Sound.MusicW.AudioContext
import Sound.Punctual.GL
import Sound.Punctual.Resolution
import Data.List (sort)

import Estuary.Utility
import Estuary.Widgets.Navigation
import Estuary.Render.WebDirt
import Estuary.Render.SuperDirt
import Estuary.Widgets.WebSocket
import Estuary.Types.Definition
import Estuary.Types.Request as Request
import Estuary.Types.Response as Response
import Estuary.Types.Hint
import Estuary.Types.Tempo
import Estuary.Widgets.Reflex
import Estuary.Types.RenderInfo
import Estuary.Render.DynamicsMode
import Estuary.Widgets.Header
import Estuary.Widgets.Footer
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Render.Renderer
import Estuary.Widgets.Terminal
import Estuary.Widgets.Reflex
import Estuary.Widgets.Sidebar
import Estuary.Resources.AudioResource
import Estuary.Types.AudioMeta
import Estuary.Resources.Loadable
import Estuary.Resources.ResourceList
import Estuary.Resources
import Estuary.Types.ResourceMeta
import Estuary.Types.ResourceType
import Estuary.Types.ResourceOp
import Estuary.Widgets.W as W
import Estuary.Render.R as R
import Estuary.Render.MainBus
import Estuary.Client.Settings as Settings
import Estuary.Types.LogEntry
import Estuary.Render.RenderOp as RenderOp

keyboardHintsCatcher :: MonadWidget t m => R.RenderEnvironment -> Settings -> m ()
keyboardHintsCatcher rEnv settings = mdo
  (theElement,_) <- elClass' "div" "" $ estuaryWidget rEnv settings keyboardShortcut
  let e = HTMLDivElement $ pToJSVal $ _el_element theElement
  togTerminal <- (24 <$) <$> catchKeyboardShortcut e 24 True True
  togStats <- (12 <$) <$> catchKeyboardShortcut e 12 True True
  let keyboardShortcut = leftmost [togTerminal,togStats]
  return ()


keyboardHintsW :: (Reflex t, MonadFix m, MonadHold t m) => Event t Int -> W t m ()
keyboardHintsW x = do
  toggleTerminalVisible $ ffilter (==24) x
  toggleStatsVisible $ ffilter (==12) x


settingsForWidgets :: (Monad m, PerformEvent t m, Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m, MonadIO (Performable m)) => R.RenderEnvironment -> Settings -> Event t [Hint] -> m (Dynamic t Settings)
settingsForWidgets rEnv iSettings hints = do
  settingsChange <- delay 0.025 $ fmapMaybe hintsToSettingsChange hints
  settings <- foldDyn ($) iSettings settingsChange
  performEvent_ $ fmap (liftIO . R.updateSettings rEnv) $ updated settings
  return settings

hintsToSettingsChange :: [Hint] -> Maybe (Settings -> Settings)
hintsToSettingsChange = g . catMaybes . fmap f
  where
    f (ChangeSettings x) = Just x
    f _ = Nothing
    g [] = Nothing
    g (x:[]) = Just x
    g xs = Just $ foldl1 (.) xs


estuaryWidget :: MonadWidget t m => R.RenderEnvironment -> Settings -> Event t Int -> m ()
estuaryWidget rEnv iSettings keyboardShortcut = divClass "estuary" $ mdo

  settings <- settingsForWidgets rEnv iSettings hints
  vidDiv <- cinecer0Widget settings
  punctualZIndex' <- holdUniqDyn $ fmap Settings.punctualZIndex settings
  cvsElement <- canvasWidget settings punctualZIndex' -- canvas for Punctual
  glCtx <- liftIO $ newGLContext cvsElement
  improvizZIndex' <- holdUniqDyn $ fmap Settings.improvizZIndex settings
  iCanvas <- canvasWidget settings improvizZIndex' -- canvas for Improviz
  hydraZIndex' <- holdUniqDyn $ fmap Settings.hydraZIndex settings
  hCanvas <- canvasWidget settings hydraZIndex' -- canvas for Hydra

  liftIO $ forkRenderThreads rEnv iSettings vidDiv cvsElement glCtx hCanvas iCanvas

  rInfo <- pollRenderInfo rEnv
  resourceMaps <- dynamicResourceMaps rEnv
  ensembleC <- maintainEnsembleC (resources rEnv) allRequests responseDown
  ensList <- maintainEnsembleList responseDown
  resError <- maintainResponseError responseDown
  log <- maintainLog hints responseDown

  -- four GUI components: header, main (navigation), terminal, footer
  let wEnv = WidgetEnvironment {
    _renderEnvironment = rEnv,
    _renderInfo = rInfo,
    _resourceMaps = resourceMaps,
    W._settings = settings,
    _serverInfo = sInfo,
    _ensembleC = ensembleC,
    _ensembleList = ensList,
    _responseError = resError,
    _log = log
    }

  ((responseDown,sInfo),localHints) <- runW wEnv $ do
    (responseDown,sInfo) <- estuaryWebSocket requestsToSend
    keyboardHintsW keyboardShortcut
    header
    divClass "page ui-font" $ do
      navigation
      sv <- W.sideBarVisible
      hideableWidget sv "sidebar" sidebarWidget
    tv <- W.terminalVisible
    hideableWidget' tv $ terminalWidget
    footer
    return (responseDown,sInfo)

  let remoteHints = fmap responseToHints responseDown
  let hints = mergeWith (++) [localHints,remoteHints]
  let willSendRequestsToServer = current $ fmap inAnEnsemble ensembleC
  let allRequests = fmap hintsToRequests localHints
  let requestsToSend = gate willSendRequestsToServer allRequests

  -- perform Hints and Settings changes as IO
  performRenderOps rEnv hints
  performWebDirtHints (R.webDirt rEnv) hints
  performDelay rEnv settings
  performDynamicsMode rEnv settings
  performPunctualAudioInputMode rEnv settings
  performTheme settings
  performSuperDirt rEnv settings

  return ()


maintainEnsembleList :: MonadWidget t m => Event t Response -> m (Dynamic t [Text])
maintainEnsembleList responseDown = holdDyn [] $ fmapMaybe f responseDown
  where
    f (EnsembleList x) = Just x
    f _ = Nothing

maintainResponseError :: MonadWidget t m => Event t Response -> m (Dynamic t (Maybe Text))
maintainResponseError responseDown = do
  let f (Error x) = Just x
      f _ = Nothing
  let g (OK x) = Just x
      g _ = Nothing
  let errors = Just <$> fmapMaybe f responseDown
  let oks = Nothing <$ fmapMaybe g responseDown
  holdDyn Nothing $ leftmost [errors,oks]


maintainLog :: MonadWidget t m => Event t [Hint] -> Event t Response -> m (Dynamic t [LogEntry])
maintainLog hints response = do
  -- where log messages come from:
  -- currently: responses to terminal commands (including, but not only, error messages) (Hints, aka localLog)
  -- currently: chat messages received from the server when in a collaborative ensemble (Response, aka ensembleLog)
  -- future possibility: critical messages from the rendering or resource systems (also localLog)
  -- future possibility: server broadcast messages
  -- these are maintained separately and then combined; for example, when ensemble changes, chat history changes
  let justEnsembleLog (EnsembleLog x) = Just x
      justEnsembleLog _ = Nothing
  let chatsReceived = fmap (:) $ fmapMaybe justEnsembleLog response
  let leftEnsemble = never -- PLACEHOLDER
  ensembleLog <- foldDyn ($) [] $ leftmost [chatsReceived,leftEnsemble]
  localLogEntries <- performEvent $ fmap localLogsFromHints hints
  localLog <- foldDyn (++) [] $ localLogEntries
  let combinedLog = (++) <$> ensembleLog <*> localLog
  pure $ fmap (reverse . sort) combinedLog


localLogsFromHints :: MonadIO m => [Hint] -> m [LogEntry]
localLogsFromHints hs = do
  let f (LocalLog x) = Just x
      f _ = Nothing
  let txts = Data.Maybe.mapMaybe f hs
  now <- liftIO $ getCurrentTime
  let g x = LogEntry { logEntryTime = now, logEntrySender = "", logEntryText = x }
  pure $ fmap g txts


maintainEnsembleC :: MonadWidget t m => Resources -> Event t [Request] -> Event t Response -> m (Dynamic t EnsembleC)
maintainEnsembleC res requests response = mdo
  now <- liftIO $ getCurrentTime
  let initialEnsembleC = emptyEnsembleC now
  let requestChange = fmap (requestsToEnsembleC res) requests -- :: Event t (EnsembleC -> m EnsembleC)
  let responseChange = fmap responseToEnsembleC response -- :: Event t (EnsembleC -> m EnsembleC)
  let allChange = mergeWith (\x y ensC -> x ensC >>= y) [requestChange,responseChange]
  updatedEnsembleC <- performEvent $ attachWith (&) (current r) allChange
  r <- holdDyn initialEnsembleC updatedEnsembleC
  pure r

cinecer0Widget :: MonadWidget t m => Dynamic t Settings.Settings -> m HTMLDivElement
cinecer0Widget settings = do
  let canvasVisible = Settings.canvasOn <$> settings
  let canvasVisible' = fmap (("visibility:" <>)  . bool "hidden" "visible") canvasVisible
  dynZIndex <- holdUniqDyn $ fmap Settings.cineCer0ZIndex settings
  let dynZIndex' = fmap (T.pack . show) dynZIndex
  let dynAttrs = mconcat [dynAttr "class" (constDyn "canvas-or-svg-display"), dynAttr "style" (constDyn "z-index: " <> dynZIndex' <> constDyn ";" <> canvasVisible' <> constDyn ";")] -- :: Dynamic t (Map Text Text)
  res <- fmap pixels <$> (holdUniqDyn $ fmap Settings.resolution settings)
  let resMap = fmap (\(x,y) -> fromList [("width",showt (x::Int)),("height",showt (y::Int))]) res
  let attrs = (<>) <$> dynAttrs <*> resMap
  liftM (uncheckedCastTo HTMLDivElement .  _element_raw . fst) $ elDynAttr' "div" attrs $ return ()

canvasWidget :: MonadWidget t m => Dynamic t Settings -> Dynamic t Int -> m HTMLCanvasElement
canvasWidget settings dynZIndex = do
  let canvasVisible = Settings.canvasOn <$> settings
  let canvasVisible' = fmap (("visibility:" <>)  . bool "hidden" "visible") canvasVisible
  let dynZIndex' = fmap (T.pack . show) dynZIndex -- :: Dynamic t Text
  let dynAttrs = mconcat [dynAttr "class" (constDyn "canvas-or-svg-display"), dynAttr "style" (constDyn "z-index: " <> dynZIndex' <> constDyn ";" <> canvasVisible' <> constDyn ";")] -- :: Dynamic t (Map Text Text)
  res <- fmap pixels <$> (holdUniqDyn $ fmap Settings.resolution settings)
  let resMap = fmap (\(x,y) -> fromList [("width",showt (x::Int)),("height",showt (y::Int))]) res -- :: Dynamic t (Map Text Text)
  let attrs = (<>) <$> dynAttrs <*> resMap
  liftM (uncheckedCastTo HTMLCanvasElement .  _element_raw . fst) $ elDynAttr' "canvas" attrs $ return ()


-- every 1.02 seconds, read the RenderInfo MVar to get load and audio level information back from the rendering/animation threads
pollRenderInfo :: (Monad m, MonadIO m, PostBuild t m, Reflex t, MonadHold t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m), MonadFix m) => R.RenderEnvironment -> m (Dynamic t RenderInfo)
pollRenderInfo rEnv = do
  now <- liftIO $ getCurrentTime
  riInitial <- liftIO $ readMVar (renderInfo rEnv)
  ticks <- tickLossy (1.02::NominalDiffTime) now
  newInfo <- performEvent $ fmap (liftIO . const (readMVar $ renderInfo rEnv)) ticks
  holdDyn riInitial newInfo


dynamicResourceMaps :: (MonadIO m, Reflex t, MonadHold t m, TriggerEvent t m) => R.RenderEnvironment -> m (Dynamic t ResourceMaps)
dynamicResourceMaps rEnv = do
  (resourceMapsEvent,resourceMapsCallback) <- newTriggerEvent
  setResourcesUpdatedCallback (R.resources rEnv) resourceMapsCallback
  holdDyn emptyResourceMaps resourceMapsEvent


performSuperDirt :: (Monad m, MonadSample t m, Reflex t, MonadIO m, MonadHold t m, PerformEvent t m, MonadIO (Performable m), MonadFix m) => R.RenderEnvironment -> Dynamic t Settings -> m ()
performSuperDirt rEnv settings = do
  let sd = superDirt rEnv
  iSettings <- sample $ current settings
  liftIO $ setActive sd (Settings.superDirtOn iSettings)
  sdOn <- holdUniqDyn $ fmap Settings.superDirtOn settings
  performEvent_ $ fmap (liftIO . setActive sd) $ updated sdOn

performTheme :: (Monad m, Reflex t, MonadHold t m, PerformEvent t m, MonadIO (Performable m), MonadFix m) => Dynamic t Settings -> m ()
performTheme settings = do
  themeChanged <- liftM updated $ holdUniqDyn $ fmap Settings.theme settings
  performEvent_ $ fmap (liftIO . Settings.setThemeIO) themeChanged

performDynamicsMode :: (Monad m, Reflex t, MonadHold t m, PerformEvent t m, MonadIO (Performable m), MonadFix m) => R.RenderEnvironment -> Dynamic t Settings -> m ()
performDynamicsMode rEnv settings = do
  dynamicsModeChanged <- liftM updated $ holdUniqDyn $ fmap Settings.dynamicsMode settings
  performEvent_ $ fmap (liftIO . changeDynamicsMode (R.mainBus rEnv)) dynamicsModeChanged

performPunctualAudioInputMode :: (Monad m, Reflex t, MonadHold t m, PerformEvent t m, MonadIO (Performable m), MonadFix m) => R.RenderEnvironment -> Dynamic t Settings -> m ()
performPunctualAudioInputMode rEnv settings = do
  punctualAudioInputChanged <- liftM updated $ holdUniqDyn $ fmap Settings.punctualAudioInputMode settings
  performEvent_ $ fmap (liftIO . changePunctualAudioInputMode (R.mainBus rEnv)) punctualAudioInputChanged

performDelay :: (Reflex t, PerformEvent t m, MonadIO (Performable m), MonadHold t m, MonadFix m) => R.RenderEnvironment -> Dynamic t Settings -> m ()
performDelay rEnv settings = do
  let nodes = R.mainBus rEnv
  delTime <- holdUniqDyn $ fmap Settings.globalAudioDelay settings
  performEvent_ $ fmap (liftIO . changeDelay nodes) $ updated delTime

performMonitorInput :: (Reflex t, PerformEvent t m, MonadIO (Performable m), MonadHold t m, MonadFix m) => R.RenderEnvironment -> Dynamic t Settings -> m ()
performMonitorInput rEnv settings = do
  let nodes = R.mainBus rEnv
  maybeDouble <- holdUniqDyn $ fmap Settings.monitorInput settings
  performEvent_ $ fmap (liftIO . changeMonitorInput nodes) $ updated maybeDouble

performRenderOps :: (Reflex t, PerformEvent t m, MonadIO (Performable m)) => RenderEnvironment -> Event t [Hint] -> m ()
performRenderOps rEnv hints = do
  let ops = fmap hintsToRenderOps hints
  performEvent_ $ fmap (putRenderOps rEnv) ops

-- TODO: MUST be optimized so that zones are not written when changes don't involve evaluation...
-- (otherwise every bit of typing leads to potential evaluation)

hintsToRenderOps :: [Hint] -> [RenderOp]
hintsToRenderOps = concat . fmap f
  where
    f (Request LeaveEnsemble) = [RenderOp.ResetZones]
    f (Request (Request.WriteTempo t)) = [RenderOp.WriteTempo t]
    f (Request (Request.WriteZone n v)) = [RenderOp.WriteZone n v]
    f (Request (Request.ResetZones)) = [RenderOp.ResetZones]
    f (Request (Request.Reset t)) = [RenderOp.ResetZones,RenderOp.WriteTempo t]
    f _ = []
