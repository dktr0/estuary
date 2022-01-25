{-# LANGUAGE RecursiveDo, JavaScriptFFI, OverloadedStrings #-}

module Estuary.Widgets.Estuary where

import Control.Monad (liftM)

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response,getKeyEvent,preventDefault,append)
import Reflex.Dom.Contrib.KeyEvent
import Reflex.Dom.Old
import Reflex.Dynamic
import Data.Time
import Data.Map
import Data.Maybe
import Text.Read
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar
import GHCJS.Types
import GHCJS.DOM.Types (uncheckedCastTo,HTMLCanvasElement(..),HTMLDivElement(..))
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
import Estuary.Types.Context
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

keyboardHintsCatcher :: MonadWidget t m => R.RenderEnvironment -> Settings -> MVar Context -> MVar RenderInfo -> m ()
keyboardHintsCatcher rEnv settings ctxM riM = mdo
  (theElement,_) <- elClass' "div" "" $ estuaryWidget rEnv settings ctxM riM keyboardShortcut
  let e = _el_element theElement
  e' <- wrapDomEvent (e) (elementOnEventName Keypress) $ do
    y <- getKeyEvent
    if (isJust $ keyEventToHint y) then (preventDefault >> return (keyEventToHint y)) else return Nothing
  let keyboardShortcut = fmapMaybe id e'
  return ()


-- uhoh: not sure if keyboard short cuts are working, or even what keys are represented by 24 and 12!...
keyEventToHint :: KeyEvent -> Maybe Int
keyEventToHint x
  | (keShift x == True) && (keCtrl x == True) && (keKeyCode x == 24) = Just 24 -- toggle terminal
  | (keShift x == True) && (keCtrl x == True) && (keKeyCode x == 12) = Just 12 -- toggle statistics
keyEventToHint _ = Nothing

keyboardHintsW :: MonadWidget t m => Event t Int -> W t m ()
keyboardHintsW x = do
  toggleTerminalVisible $ ffilter (==24) x
  toggleStatsVisible $ ffilter (==12) x


settingsForWidgets :: MonadWidget t m => R.RenderEnvironment -> Settings -> Event t [Hint] -> m (Dynamic t Settings)
settingsForWidgets rEnv iSettings hints = do
  settingsChange <- delay 0.025 $ fmapMaybe hintsToSettingsChange hints
  settings <- foldDyn ($) iSettings settingsChange
  performEvent_ $ fmap (liftIO . R.updateSettings rEnv) $ updated settings
  return settings


estuaryWidget :: MonadWidget t m => R.RenderEnvironment -> Settings -> MVar Context -> MVar RenderInfo -> Event t Int -> m ()
estuaryWidget rEnv iSettings ctxM riM keyboardShortcut = divClass "estuary" $ mdo

  settings <- settingsForWidgets rEnv iSettings hints

  cinecer0Widget settings ctxM ctx -- div for cinecer0 shared with render threads through Context MVar, this needs to be first in this action
  punctualZIndex' <- holdUniqDyn $ fmap Settings.punctualZIndex settings
  cvsElement <- canvasWidget settings punctualZIndex' ctx -- canvas for Punctual
  glCtx <- liftIO $ newGLContext cvsElement
  improvizZIndex' <- holdUniqDyn $ fmap Settings.improvizZIndex settings
  iCanvas <- canvasWidget settings improvizZIndex' ctx -- canvas for Improviz
  hydraZIndex' <- holdUniqDyn $ fmap Settings.hydraZIndex settings
  hCanvas <- canvasWidget settings hydraZIndex' ctx -- canvas for Hydra

  iCtx <- liftIO $ readMVar ctxM
  ctx <- foldDyn ($) iCtx contextChange -- dynamic context; near the top here so it is available for everything else

  liftIO $ forkRenderThreads rEnv iSettings ctxM cvsElement glCtx hCanvas riM

  performContext ctxM ctx -- perform all IO actions consequent to Context changing
  rInfo <- pollRenderInfo riM -- dynamic render info (written by render threads, read by widgets)
  (deltasDown',wsCtxChange,wsHints) <- estuaryWebSocket ctx rInfo requestsUp
  let responsesFromEnsembleRequests = fmap ((:[]) . EnsembleResponse) $ fmapMaybe ensembleRequestsToResponses commandEnsembleRequests
  let deltasDownAlt = mergeWith (++) [fmap (:[]) deltasDown',responsesFromHints,responsesFromEnsembleRequests]
  let deltasDown = mergeWith (++) [fmap (:[]) deltasDown',responsesFromHints]

  let ensembleCDyn = fmap ensembleC ctx

  -- resourceMaps :: Dynamic t ResourceMaps, updated by callback events from the Resources system
  (resourceMapsEvent,resourceMapsCallback) <- newTriggerEvent
  setResourcesUpdatedCallback (R.resources rEnv) resourceMapsCallback
  resourceMaps <- holdDyn emptyResourceMaps resourceMapsEvent

  -- four GUI components: header, main (navigation), terminal, footer
  let wEnv = WidgetEnvironment {
    _renderEnvironment = rEnv,
    _context = ctx,
    _renderInfo = rInfo,
    _resourceMaps = resourceMaps,
    W._settings = settings
    }

  (_,keyboardHints) <- runW wEnv $ keyboardHintsW keyboardShortcut

  (_,headerHints) <- runW wEnv header

  ((requests, ensembleRequestFromPage), sidebarChange, hintsFromPage) <- divClass "page ui-font" $ do
    (navRequests,pageHints) <- runW wEnv $ navigation deltasDownAlt
    (ctxChange,sidebarHints) <- runW wEnv $ do
      sv <- W.sideBarVisible
      hideableWidget sv "sidebar" $ sidebarWidget ctx rInfo
    let mergedHints = mergeWith (++) [pageHints, sidebarHints]
    return (navRequests,ctxChange,mergedHints)

  (command,_) <- runW wEnv $ do
    tv <- W.terminalVisible
    hideableWidget' tv $ terminalWidget deltasDown hints

  (_,footerHints) <- runW wEnv footer

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
  performDelayHints rEnv hints
  performDynamicsMode rEnv settings
  performPunctualAudioInputMode rEnv settings
  performTheme settings

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
    f (ZoneHint n d) = Just (WriteZone n d)
    f _ = Nothing

hintsToResponses :: [Hint] -> [Estuary.Types.Response.Response]
hintsToResponses = catMaybes . fmap f
  where
    f (ZoneHint n d) = Just (EnsembleResponse (ZoneRcvd n d))
    f _ = Nothing

hintsToSettingsChange :: [Hint] -> Maybe (Settings -> Settings)
hintsToSettingsChange = g . catMaybes . fmap f
  where
    f (ChangeSettings x) = Just x
    f _ = Nothing
    g [] = Nothing
    g (x:[]) = Just x
    g xs = Just $ foldl1 (.) xs

cinecer0Widget :: MonadWidget t m => Dynamic t Settings.Settings -> MVar Context -> Dynamic t Context -> m ()
cinecer0Widget settings ctxM ctx = do
  ic0 <- liftIO $ takeMVar ctxM
  let canvasVisible = Settings.canvasOn <$> settings
  let canvasVisible' = fmap (("visibility:" <>)  . bool "hidden" "visible") canvasVisible
  dynZIndex <- holdUniqDyn $ fmap Settings.cineCer0ZIndex settings
  let dynZIndex' = fmap (T.pack . show) dynZIndex
  let dynAttrs = mconcat [dynAttr "class" (constDyn "canvas-or-svg-display"), dynAttr "style" (constDyn "z-index: " <> dynZIndex' <> constDyn ";" <> canvasVisible' <> constDyn ";")] -- :: Dynamic t (Map Text Text)
  res <- fmap pixels <$> (holdUniqDyn $ fmap Settings.resolution settings)
  let resMap = fmap (\(x,y) -> fromList [("width",showt (x::Int)),("height",showt (y::Int))]) res
  let attrs = (<>) <$> dynAttrs <*> resMap
  videoDiv <- liftM (uncheckedCastTo HTMLDivElement .  _element_raw . fst) $ elDynAttr' "div" attrs $ return ()
  let ic = ic0 { videoDivElement = Just videoDiv }
  liftIO $ putMVar ctxM ic

canvasWidget :: MonadWidget t m => Dynamic t Settings -> Dynamic t Int -> Dynamic t Context -> m HTMLCanvasElement
canvasWidget settings dynZIndex ctx = do
  let canvasVisible = Settings.canvasOn <$> settings
  let canvasVisible' = fmap (("visibility:" <>)  . bool "hidden" "visible") canvasVisible
  let dynZIndex' = fmap (T.pack . show) dynZIndex -- :: Dynamic t Text
  let dynAttrs = mconcat [dynAttr "class" (constDyn "canvas-or-svg-display"), dynAttr "style" (constDyn "z-index: " <> dynZIndex' <> constDyn ";" <> canvasVisible' <> constDyn ";")] -- :: Dynamic t (Map Text Text)
  res <- fmap pixels <$> (holdUniqDyn $ fmap Settings.resolution settings)
  let resMap = fmap (\(x,y) -> fromList [("width",showt (x::Int)),("height",showt (y::Int))]) res -- :: Dynamic t (Map Text Text)
  let attrs = (<>) <$> dynAttrs <*> resMap
  liftM (uncheckedCastTo HTMLCanvasElement .  _element_raw . fst) $ elDynAttr' "canvas" attrs $ return ()


-- every 1.02 seconds, read the RenderInfo MVar to get load and audio level information back from the rendering/animation threads
pollRenderInfo :: MonadWidget t m => MVar RenderInfo -> m (Dynamic t RenderInfo)
pollRenderInfo riM = do
  now <- liftIO $ getCurrentTime
  riInitial <- liftIO $ readMVar riM
  ticks <- tickLossy (1.02::NominalDiffTime) now
  newInfo <- performEvent $ fmap (liftIO . const (readMVar riM)) ticks
  holdDyn riInitial newInfo


performContext :: MonadWidget t m => MVar Context -> Dynamic t Context -> m ()
performContext cMvar cDyn = do
  iCtx <- sample $ current cDyn
  performEvent_ $ fmap (liftIO . (\x -> swapMVar cMvar x >> return ())) $ updated cDyn -- transfer whole Context for render/animation threads

  -- when the superDirt flag changes, make it so
  -- TODO: this is broken, needs to be re-implemented!!!
  -- let sd = superDirt rEnv
  -- sdOn <- holdUniqDyn $ fmap superDirtOn cDyn
  -- performEvent_ $ fmap (liftIO . setActive sd) $ updated sdOn

performTheme :: MonadWidget t m => Dynamic t Settings -> m ()
performTheme settings = do
  themeChanged <- liftM updated $ holdUniqDyn $ fmap Settings.theme settings
  performEvent_ $ fmap (liftIO . js_setThemeHref) themeChanged

foreign import javascript safe
  "document.getElementById('estuary-current-theme').setAttribute('href', $1);"
  js_setThemeHref :: Text -> IO ()

performDynamicsMode :: MonadWidget t m => R.RenderEnvironment -> Dynamic t Settings -> m ()
performDynamicsMode rEnv settings = do
  dynamicsModeChanged <- liftM updated $ holdUniqDyn $ fmap Settings.dynamicsMode settings
  performEvent_ $ fmap (liftIO . changeDynamicsMode (R.mainBus rEnv)) dynamicsModeChanged

performPunctualAudioInputMode :: MonadWidget t m => R.RenderEnvironment -> Dynamic t Settings -> m ()
performPunctualAudioInputMode rEnv settings = do
  punctualAudioInputChanged <- liftM updated $ holdUniqDyn $ fmap Settings.punctualAudioInputMode settings
  performEvent_ $ fmap (liftIO . changePunctualAudioInputMode (R.mainBus rEnv)) punctualAudioInputChanged

-- TODO: this still needs to be refactored to use new Settings type, not Hint directly
performDelayHints :: MonadWidget t m => R.RenderEnvironment -> Event t [Hint] -> m ()
performDelayHints rEnv hs = do
  let nodes = R.mainBus rEnv
  let newDelayTime = fmapMaybe justGlobalDelayTime hs
  performEvent_ $ fmap (liftIO . changeDelay nodes) newDelayTime
  where
    f (SetGlobalDelayTime x) = Just x
    f _ = Nothing


commandToRequest :: Terminal.Command -> Maybe Request
commandToRequest (Terminal.DeleteThisEnsemble pwd) = Just (DeleteThisEnsemble pwd)
commandToRequest (Terminal.DeleteEnsemble eName pwd) = Just (DeleteEnsemble eName pwd)
commandToRequest _ = Nothing
