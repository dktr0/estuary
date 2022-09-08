{-# LANGUAGE RecursiveDo, JavaScriptFFI, OverloadedStrings, FlexibleContexts #-}

module Estuary.Widgets.Estuary where

import Control.Monad (liftM)

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response,append)
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
import GHCJS.DOM.Types hiding (Event,Request)
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

import Estuary.Utility
import Estuary.Widgets.Navigation
import Estuary.Render.WebDirt
import Estuary.Render.SuperDirt
import Estuary.Widgets.WebSocket
import Estuary.Types.Definition
import Estuary.Types.Request
import Estuary.Types.EnsembleRequest
import Estuary.Types.Response
import Estuary.Types.EnsembleResponse
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
import qualified Estuary.Types.Terminal as Terminal
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



  -- four GUI components: header, main (navigation), terminal, footer
  let wEnv = WidgetEnvironment {
    _renderEnvironment = rEnv,
    _renderInfo = rInfo,
    _resourceMaps = resourceMaps,
    W._settings = settings,
    _serverInfo = serverInfo,
    _ensembleC = ensembleC
    }

  (_,keyboardAndHeaderHints) <- runW wEnv $ do
    (responseDown,sInfo) <- estuaryWebSocket requestsUp
    keyboardHintsW keyboardShortcut
    header
    navRequests <- divClass "page ui-font" $ do
      navRequests <- navigation deltasDownAlt
      sv <- W.sideBarVisible
      hideableWidget sv "sidebar" sideBarWidget
      return navRequests
    command <- do -- :: Event t Command
      tv <- W.terminalVisible
      hideableWidget' tv $ terminalWidget responseDown hints
    footer


    {-
    let responsesFromEnsembleRequests = fmap ((:[]) . EnsembleResponse) $ fmapMaybe ensembleRequestsToResponses commandEnsembleRequests
    let deltasDownAlt = mergeWith (++) [fmap (:[]) deltasDown',responsesFromHints,responsesFromEnsembleRequests]
    let deltasDown = mergeWith (++) [fmap (:[]) deltasDown',responsesFromHints]
    -}

  commandEnsembleRequests <- performEvent $ attachWithMaybe commandToEnsembleRequest (current ensembleCDyn) command
  let ensembleRequests = leftmost [commandEnsembleRequests, ensembleRequestFromPage, ensembleRequestsFromHints]
  performEvent_ $ fmap (ensembleRequestIO $ R.resources rEnv) ensembleRequests
  let commandRequests = fmapMaybe commandToRequest command

  -- changes to EnsembleC within Context, and to Context
  let commandChange = fmap commandToStateChange command
  let ensembleRequestChange = fmap requestToStateChange ensembleRequests
  let ensembleResponses = fmapMaybe justEnsembleResponses deltasDown
  let ensembleResponseChange0 = fmap ((Prelude.foldl (.) id) . fmap responseToStateChange) deltasDown
  let ensembleResponseChange1 = fmap ((Prelude.foldl (.) id) . fmap ensembleResponseToStateChange) ensembleResponses
  performEvent_ $ fmap (mapM_ (ensembleResponseIO $ R.resources rEnv)) ensembleResponses
  let ensembleChange = fmap modifyEnsembleC $ mergeWith (.) [commandChange,ensembleRequestChange,ensembleResponseChange0,ensembleResponseChange1]
  let ccChange = fmap (setClientCount . fst) $ fmapMaybe justServerInfo deltasDown'
  let contextChange = mergeWith (.) [ensembleChange, ccChange, wsCtxChange, sidebarChange]

  -- hints
  let commandHint = attachWithMaybe commandToHint (current ensembleCDyn) command
  let hints = mergeWith (++) [hintsFromPage, fmap (:[]) commandHint, footerHints, keyboardHints, pure <$> wsHints, headerHints] -- Event t [Hint]
  let ensembleRequestsFromHints = fmapMaybe lastOrNothing $ fmap hintsToEnsembleRequests hints
  let responsesFromHints = fmapMaybe listOrNothing $ fmap hintsToResponses hints
  performHints (R.webDirt rEnv) hints
  performDelayHints rEnv settings
  performDynamicsMode rEnv settings
  performPunctualAudioInputMode rEnv settings
  performTheme settings
  performSuperDirt rEnv settings

  -- requests up to server
  let ensembleRequestsUp = gate (current $ fmap (inAnEnsemble . ensembleC) ctx) $ fmap EnsembleRequest ensembleRequests
  let ensembleRequestsUp' = fmap (:[]) ensembleRequestsUp
  let requests' = fmap (:[]) $ requests
  let requests'' = fmap (:[]) $ commandRequests
  let requestsUp = mergeWith (++) [ensembleRequestsUp',requests',requests'']

  return ()

hintsToEnsembleRequests :: [Hint] -> [EnsembleRequest]
hintsToEnsembleRequests = catMaybes . fmap f
  where
    f (RequestHint (EnsembleRequest x)) = Just x
    f _ = Nothing

hintsToResponses :: [Hint] -> [Estuary.Types.Response.Response]
hintsToResponses = catMaybes . fmap f
  where
    f (RequestHint (EnsembleRequest (WriteZone n d))) = Just (EnsembleResponse (ZoneRcvd n d))
    f _ = Nothing



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


dynamicResourceMaps :: (Monad m, Reflex t) => R.RenderEnvironment -> m (Dynamic t ResourceMaps)
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

performDelayHints :: (Reflex t, PerformEvent t m, MonadIO (Performable m)) => R.RenderEnvironment -> Dynamic t Settings -> m ()
performDelayHints rEnv settings = do
  let nodes = R.mainBus rEnv
  delTime <- holdUniqDyn $ fmap Settings.globalAudioDelay settings
  performEvent_ $ fmap (liftIO . changeDelay nodes) $ updated delTime


commandToRequest :: Terminal.Command -> Maybe Request
commandToRequest (Terminal.DeleteThisEnsemble pwd) = Just (DeleteThisEnsemble pwd)
commandToRequest (Terminal.DeleteEnsemble eName pwd) = Just (DeleteEnsemble eName pwd)
commandToRequest _ = Nothing
