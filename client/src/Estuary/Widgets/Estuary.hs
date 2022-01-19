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
import Estuary.Widgets.W
import Estuary.Widgets.Sidebar
import Estuary.Resources.AudioResource
import Estuary.Types.AudioMeta
import Estuary.Resources.Loadable
import Estuary.Resources.ResourceList
import Estuary.Resources
import Estuary.Types.ResourceMeta
import Estuary.Types.ResourceType
import Estuary.Types.ResourceOp
import Estuary.Render.R

keyboardHintsCatcher :: MonadWidget t m => MVar Context -> MVar RenderInfo -> m ()
keyboardHintsCatcher ctxM riM = mdo
  (theElement,_) <- elClass' "div" "" $ estuaryWidget ctxM riM keyboardHints
  let e = _el_element theElement
  e' <- wrapDomEvent (e) (elementOnEventName Keypress) $ do
    y <- getKeyEvent
    if (isJust $ keyEventToHint y) then (preventDefault >> return (keyEventToHint y)) else return Nothing
  let e'' = fmapMaybe id e'
  let keyboardHints = fmap (:[]) $ e''
  return ()

keyEventToHint :: KeyEvent -> Maybe Hint
keyEventToHint x
  | (keShift x == True) && (keCtrl x == True) && (keKeyCode x == 24) = Just ToggleTerminal
  | (keShift x == True) && (keCtrl x == True) && (keKeyCode x == 12) = Just ToggleStats
keyEventToHint _ = Nothing

estuaryWidget :: MonadWidget t m => MVar Context -> MVar RenderInfo -> Event t [Hint] -> m ()
estuaryWidget ctxM riM keyboardHints = divClass "estuary" $ mdo


  canvasVisible <- holdDyn True $ fmapMaybe hintsToCanvasVisibility hints
  cinecer0Widget canvasVisible ctxM ctx -- div for cinecer0 shared with render threads through Context MVar, this needs to be first in this action
  punctualZIndex' <- holdUniqDyn $ fmap punctualZIndex ctx
  cvsElement <- canvasWidget canvasVisible punctualZIndex' ctx -- canvas for Punctual
  glCtx <- liftIO $ newGLContext cvsElement
  improvizZIndex' <- holdUniqDyn $ fmap improvizZIndex ctx
  iCanvas <- canvasWidget canvasVisible improvizZIndex' ctx -- canvas for Improviz
  hydraZIndex' <- holdUniqDyn $ fmap hydraZIndex ctx
  hCanvas <- canvasWidget canvasVisible hydraZIndex' ctx -- canvas for Hydra

  iCtx <- liftIO $ readMVar ctxM
  ctx <- foldDyn ($) iCtx contextChange -- dynamic context; near the top here so it is available for everything else

  rEnv <- liftIO $ forkRenderThreads ctxM cvsElement glCtx hCanvas riM

  performContext rEnv ctxM ctx -- perform all IO actions consequent to Context changing
  rInfo <- pollRenderInfo riM -- dynamic render info (written by render threads, read by widgets)
  (deltasDown',wsCtxChange,wsHints) <- estuaryWebSocket ctx rInfo requestsUp
  let responsesFromEnsembleRequests = fmap ((:[]) . EnsembleResponse) $ fmapMaybe ensembleRequestsToResponses commandEnsembleRequests
  let deltasDownAlt = mergeWith (++) [fmap (:[]) deltasDown',responsesFromHints,responsesFromEnsembleRequests]
  let deltasDown = mergeWith (++) [fmap (:[]) deltasDown',responsesFromHints]

  let ensembleCDyn = fmap ensembleC ctx

  -- resourceMaps :: Dynamic t ResourceMaps, updated by callback events from the Resources system
  (resourceMapsEvent,resourceMapsCallback) <- newTriggerEvent
  setResourcesUpdatedCallback (resources rEnv) resourceMapsCallback
  resourceMaps <- holdDyn emptyResourceMaps resourceMapsEvent

  -- four GUI components: header, main (navigation), terminal, footer
  let wEnv = WidgetEnvironment {
    _renderEnvironment = rEnv,
    _context = ctx,
    _renderInfo = rInfo,
    _resourceMaps = resourceMaps
    }
  (headerChange,headerHints) <- runW wEnv header
  ((requests, ensembleRequestFromPage), sidebarChange, hintsFromPage) <- divClass "page ui-font" $ do
    let sidebarToggle = ffilter (elem ToggleSidebar) hints
    sidebarVisible <- toggle False sidebarToggle
    (navRequests,pageHints) <- runW wEnv $ navigation deltasDownAlt
    (ctxChange,sidebarHints) <- runW wEnv $ hideableWidget sidebarVisible "sidebar" $ sidebarWidget ctx rInfo
    let mergedHints = mergeWith (++) [pageHints, sidebarHints]
    return (navRequests,ctxChange,mergedHints)
  let terminalShortcut = ffilter (elem ToggleTerminal) hints
  let terminalEvent = leftmost [() <$ terminalShortcut, terminalButton]
  terminalVisible <- toggle True terminalEvent
  (command,_) <- hideableWidget' terminalVisible $ do
    runW wEnv $ terminalWidget deltasDown hints
  (terminalButton,_) <- runW wEnv $ footer hints
  commandEnsembleRequests <- performEvent $ attachWithMaybe commandToEnsembleRequest (current ensembleCDyn) command
  let ensembleRequests = leftmost [commandEnsembleRequests, ensembleRequestFromPage, ensembleRequestsFromHints]
  performEvent_ $ fmap (ensembleRequestIO $ resources rEnv) ensembleRequests
  let commandRequests = fmapMaybe commandToRequest command

  -- changes to EnsembleC within Context, and to Context
  let commandChange = fmap commandToStateChange command
  let ensembleRequestChange = fmap requestToStateChange ensembleRequests
  let ensembleResponses = fmapMaybe justEnsembleResponses deltasDown
  let ensembleResponseChange0 = fmap ((Prelude.foldl (.) id) . fmap responseToStateChange) deltasDown
  let ensembleResponseChange1 = fmap ((Prelude.foldl (.) id) . fmap ensembleResponseToStateChange) ensembleResponses
  performEvent_ $ fmap (mapM_ (ensembleResponseIO $ resources rEnv)) ensembleResponses
  let ensembleChange = fmap modifyEnsembleC $ mergeWith (.) [commandChange,ensembleRequestChange,ensembleResponseChange0,ensembleResponseChange1]
  let ccChange = fmap (setClientCount . fst) $ fmapMaybe justServerInfo deltasDown'
  let contextChange = mergeWith (.) [ensembleChange, headerChange, ccChange, wsCtxChange, sidebarChange]

  -- hints
  let commandHint = attachWithMaybe commandToHint (current ensembleCDyn) command
  let hints = mergeWith (++) [hintsFromPage, fmap (:[]) commandHint, keyboardHints, pure <$> wsHints, headerHints] -- Event t [Hint]
  let ensembleRequestsFromHints = fmapMaybe lastOrNothing $ fmap hintsToEnsembleRequests hints
  let responsesFromHints = fmapMaybe listOrNothing $ fmap hintsToResponses hints
  performHints (webDirt rEnv) hints
  performDelayHints rEnv hints

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

hintsToCanvasVisibility :: [Hint] -> Maybe Bool
hintsToCanvasVisibility = listToMaybe . reverse . catMaybes . fmap f
  where
    f (CanvasActive x) = Just x
    f _ = Nothing

cinecer0Widget :: MonadWidget t m => Dynamic t Bool -> MVar Context -> Dynamic t Context -> m ()
cinecer0Widget isCanvasVisible ctxM ctx = do
  ic0 <- liftIO $ takeMVar ctxM
  let canvasVisible = fmap (("visibility:" <>)  . bool "hidden" "visible") isCanvasVisible
  dynZIndex <- holdUniqDyn $ fmap cineCer0ZIndex ctx
  let dynZIndex' = fmap (T.pack . show) dynZIndex
  let dynAttrs = mconcat [dynAttr "class" (constDyn "canvas-or-svg-display"), dynAttr "style" (constDyn "z-index: " <> dynZIndex' <> constDyn ";" <> canvasVisible <> constDyn ";")] -- :: Dynamic t (Map Text Text)
  res <- fmap pixels <$> (holdUniqDyn $ fmap resolution ctx)
  let resMap = fmap (\(x,y) -> fromList [("width",showt (x::Int)),("height",showt (y::Int))]) res
  let attrs = (<>) <$> dynAttrs <*> resMap
  videoDiv <- liftM (uncheckedCastTo HTMLDivElement .  _element_raw . fst) $ elDynAttr' "div" attrs $ return ()
  let ic = ic0 { videoDivElement = Just videoDiv }
  liftIO $ putMVar ctxM ic

canvasWidget :: MonadWidget t m => Dynamic t Bool -> Dynamic t Int -> Dynamic t Context -> m HTMLCanvasElement
canvasWidget isCanvasVisible dynZIndex ctx = do
  let canvasVisible = fmap (("visibility: " <>)  . bool "hidden" "visible") isCanvasVisible
  let dynZIndex' = fmap (T.pack . show) dynZIndex -- :: Dynamic t Text
  let dynAttrs = mconcat [dynAttr "class" (constDyn "canvas-or-svg-display"), dynAttr "style" (constDyn "z-index: " <> dynZIndex' <> constDyn ";" <> canvasVisible <> constDyn ";")] -- :: Dynamic t (Map Text Text)
  res <- fmap pixels <$> (holdUniqDyn $ fmap resolution ctx)
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


-- whenever the Dynamic representation of the Context changes, translate that
-- into various
performContext :: MonadWidget t m => RenderEnvironment -> MVar Context -> Dynamic t Context -> m ()
performContext rEnv cMvar cDyn = do
  iCtx <- sample $ current cDyn
  performEvent_ $ fmap (liftIO . updateContext cMvar) $ updated cDyn -- transfer whole Context for render/animation threads
  updateDynamicsModes rEnv cDyn -- when dynamics modes change, make it so
  updatePunctualAudioInputMode rEnv cDyn
  -- when the theme changes,
  t <- holdUniqDyn $ fmap theme cDyn -- Dynamic t String
  let t' = updated t -- Event t String
  changeTheme t'
  -- when the superDirt flag changes, make it so
  let sd = superDirt rEnv
  sdOn <- holdUniqDyn $ fmap superDirtOn cDyn
  performEvent_ $ fmap (liftIO . setActive sd) $ updated sdOn

updateContext :: MVar Context -> Context -> IO ()
updateContext mv x = swapMVar mv x >> return ()

updateDynamicsModes :: MonadWidget t m => RenderEnvironment -> Dynamic t Context -> m ()
updateDynamicsModes rEnv ctx = do
  dynamicsModeChanged <- liftM updated $ holdUniqDyn $ fmap dynamicsMode ctx
  performEvent_ $ fmap (liftIO . changeDynamicsMode (mainBus rEnv)) dynamicsModeChanged

updatePunctualAudioInputMode :: MonadWidget t m => RenderEnvironment -> Dynamic t Context -> m ()
updatePunctualAudioInputMode rEnv ctx = do
  punctualAudioInputChanged <- liftM updated $ holdUniqDyn $ fmap punctualAudioInputMode ctx
  performEvent_ $ fmap (liftIO . changePunctualAudioInputMode (mainBus rEnv)) punctualAudioInputChanged

performDelayHints :: MonadWidget t m => RenderEnvironment -> Event t [Hint] -> m ()
performDelayHints rEnv hs = do
  let nodes = mainBus rEnv
  let newDelayTime = fmapMaybe justGlobalDelayTime hs
  performEvent_ $ fmap (liftIO . changeDelay nodes) newDelayTime
  where
    f (SetGlobalDelayTime x) = Just x
    f _ = Nothing


changeTheme :: MonadWidget t m => Event t Text -> m ()
changeTheme newStyle = performEvent_ $ fmap (liftIO . js_setThemeHref) newStyle

foreign import javascript safe
  "document.getElementById('estuary-current-theme').setAttribute('href', $1);"
  js_setThemeHref :: Text -> IO ()

commandToRequest :: Terminal.Command -> Maybe Request
commandToRequest (Terminal.DeleteThisEnsemble pwd) = Just (DeleteThisEnsemble pwd)
commandToRequest (Terminal.DeleteEnsemble eName pwd) = Just (DeleteEnsemble eName pwd)
commandToRequest _ = Nothing
