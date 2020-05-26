{-# LANGUAGE RecursiveDo, JavaScriptFFI, OverloadedStrings #-}

module Estuary.Widgets.Estuary where

import Control.Monad (liftM)

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response,getKeyEvent,preventDefault)
import Reflex.Dom.Contrib.KeyEvent
import Reflex.Dom.Old
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
import Estuary.Types.Samples
import Estuary.Types.Tempo
import Estuary.Reflex.Utility
import Estuary.Types.RenderInfo
import Estuary.Render.DynamicsMode
import Estuary.Widgets.Header
import Estuary.Widgets.Footer
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Render.Renderer
import Estuary.Widgets.Terminal
import Estuary.Widgets.Generic
import qualified Estuary.Types.Terminal as Terminal
import Estuary.Widgets.Editor
import Estuary.Widgets.SideBar

keyboardHintsCatcher :: MonadWidget t m => ImmutableRenderContext -> MVar Context -> MVar RenderInfo -> m ()
keyboardHintsCatcher irc ctxM riM = mdo
  (theElement,_) <- elClass' "div" "" $ estuaryWidget irc ctxM riM keyboardHints
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

estuaryWidget :: MonadWidget t m => ImmutableRenderContext -> MVar Context -> MVar RenderInfo -> Event t [Hint] -> m ()
estuaryWidget irc ctxM riM keyboardHints = divClass "estuary" $ mdo

  cinecer0Widget ctxM ctx -- div for cinecer0 shared with render threads through Context MVar, this needs to be first in this action
  glCtx <- canvasWidget ctx -- canvas for Punctual
  iCtx <- liftIO $ readMVar ctxM
  ctx <- foldDyn ($) iCtx contextChange -- dynamic context; near the top here so it is available for everything else
  performContext irc ctxM ctx -- perform all IO actions consequent to Context changing
  rInfo <- pollRenderInfo riM -- dynamic render info (written by render threads, read by widgets)
  (deltasDown',wsCtxChange) <- estuaryWebSocket rInfo requestsUp
  let deltasDown = mergeWith (++) [fmap (:[]) deltasDown',responsesFromHints]

  let ensembleCDyn = fmap ensembleC ctx

  -- four GUI components: header, main (navigation), terminal, footer
  (headerChange,headerHints) <- runEditor ctx rInfo header
  ((requests, ensembleRequestFromPage), hintsFromPage) <- divClass "page ui-font" $ do
    let sidebarToggle = ffilter (elem ToggleSidebar) hints
    sidebarVisible <- toggle False sidebarToggle
    requestsAndHints <- runEditor ctx rInfo $ navigation deltasDown
    hideableWidget sidebarVisible "sidebar" $ sidebarWidget
    return requestsAndHints
  let terminalShortcut = ffilter (elem ToggleTerminal) hints
  let terminalEvent = leftmost [() <$ terminalShortcut, terminalButton]
  terminalVisible <- toggle False terminalEvent
  (command,_) <- hideableWidget' terminalVisible $ do
    runEditor ctx rInfo $ terminalWidget deltasDown hints
  (terminalButton,_) <- runEditor ctx rInfo $ footer hints
  let commandEnsembleRequests = attachWithMaybe commandToEnsembleRequest (current ensembleCDyn) command
  let ensembleRequests = leftmost [commandEnsembleRequests, ensembleRequestFromPage,ensembleRequestsFromHints]
  let commandRequests = fmapMaybe commandToRequest command

  -- map from EnsembleRequests (eg. edits) and Commands (ie. from the terminal) to

  -- changes to EnsembleC within Context, and to Context
  let commandChange = fmap commandToStateChange command
  let ensembleRequestChange = fmap requestToStateChange ensembleRequests
  let ensembleResponses = fmap justEnsembleResponses deltasDown
  let ensembleResponseChange0 = fmap ((Prelude.foldl (.) id) . fmap responseToStateChange) deltasDown
  let ensembleResponseChange1 = fmap ((Prelude.foldl (.) id) . fmap ensembleResponseToStateChange) ensembleResponses
  let ensembleChange = fmap modifyEnsembleC $ mergeWith (.) [commandChange,ensembleRequestChange,ensembleResponseChange0,ensembleResponseChange1]
  let ccChange = fmap (setClientCount . fst) $ fmapMaybe justServerInfo deltasDown'
  -- samplesLoadedEv <- loadSampleMap
  let contextChange = mergeWith (.) [ensembleChange, headerChange, ccChange, {- samplesLoadedEv, -} wsCtxChange]

  -- hints
  let commandHint = attachWithMaybe commandToHint (current ensembleCDyn) command
  let hints = mergeWith (++) [hintsFromPage, fmap (:[]) commandHint, keyboardHints, headerHints] -- Event t [Hint]
  let ensembleRequestsFromHints = fmapMaybe lastOrNothing $ fmap hintsToEnsembleRequests hints
  let responsesFromHints = fmapMaybe listOrNothing $ fmap hintsToResponses hints
  performHints (webDirt irc) hints
  performDelayHints irc hints

  -- requests up to server
  let ensembleRequestsUp = gate (current $ fmap (inAnEnsemble . ensembleC) ctx) $ fmap EnsembleRequest ensembleRequests
  let ensembleRequestsUp' = fmap (:[]) ensembleRequestsUp
  let requests' = fmap (:[]) $ requests
  let requests'' = fmap (:[]) $ commandRequests
  let requestsUp = mergeWith (++) [ensembleRequestsUp',requests',requests'']

  liftIO $ forkRenderThreads irc ctxM glCtx riM
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

cinecer0Widget :: MonadWidget t m => MVar Context -> Dynamic t Context -> m ()
cinecer0Widget ctxM ctx = do
  ic0 <- liftIO $ takeMVar ctxM
  canvasVisible <- fmap (("visibility:" <>)  . bool "hidden" "visible") <$> (holdUniqDyn $ fmap canvasOn ctx)
  let baseAttrs = ffor canvasVisible $ \x -> fromList [("class","canvas-or-svg-display"),("style","z-index: -1;" <> x <> ";")]
  res <- fmap pixels <$> (holdUniqDyn $ fmap resolution ctx)
  let resMap = fmap (\(x,y) -> fromList [("width",showt (x::Int)),("height",showt (y::Int))]) res
  let attrs = (<>) <$> baseAttrs <*> resMap
  videoDiv <- liftM (uncheckedCastTo HTMLDivElement .  _element_raw . fst) $ elDynAttr' "div" attrs $ return ()
  let ic = ic0 { videoDivElement = Just videoDiv }
  liftIO $ putMVar ctxM ic

canvasWidget :: MonadWidget t m => Dynamic t Context -> m GLContext
canvasWidget ctx = do
  canvasVisible <- fmap (("visibility:" <>)  . bool "hidden" "visible") <$> (holdUniqDyn $ fmap canvasOn ctx)
  let baseAttrs = ffor canvasVisible $ \x -> fromList [("class","canvas-or-svg-display"),("style","z-index: -2;" <> x <> ";")]
  res <- fmap pixels <$> (holdUniqDyn $ fmap resolution ctx)
  let resMap = fmap (\(x,y) -> fromList [("width",showt (x::Int)),("height",showt (y::Int))]) res
  let attrs = (<>) <$> baseAttrs <*> resMap
  canvas <- liftM (uncheckedCastTo HTMLCanvasElement .  _element_raw . fst) $ elDynAttr' "canvas" attrs $ return ()
  glc <- liftIO $ newGLContext canvas
  return glc


-- every 1.02 seconds, read the RenderInfo MVar to get load and audio level information back from the rendering/animation threads
pollRenderInfo :: MonadWidget t m => MVar RenderInfo -> m (Dynamic t RenderInfo)
pollRenderInfo riM = do
  now <- liftIO $ getCurrentTime
  riInitial <- liftIO $ readMVar riM
  ticks <- tickLossy (1.02::NominalDiffTime) now
  newInfo <- performEvent $ fmap (liftIO . const (readMVar riM)) ticks
  holdDyn riInitial newInfo

{-
-- load the sample map and issue an appropriate ContextChange event when finished
-- (if there is a better way to trigger an event from an async callback then this should be updated to reflect that)
loadSampleMap :: MonadWidget t m => m (Event t ContextChange)
loadSampleMap = do
  postBuild <- getPostBuild
  performEventAsync $ ffor postBuild $ \_ triggerEv -> liftIO $ do
    loadSampleMapAsync defaultSampleMapURL $ \maybeMap -> do
      case maybeMap of
        Nothing -> return () -- Couldn't load the map
        Just map -> triggerEv $ setSampleMap map
-}


-- whenever the Dynamic representation of the Context changes, translate that
-- into various
performContext :: MonadWidget t m => ImmutableRenderContext -> MVar Context -> Dynamic t Context -> m ()
performContext irc cMvar cDyn = do
  iCtx <- sample $ current cDyn
  performEvent_ $ fmap (liftIO . updateContext cMvar) $ updated cDyn -- transfer whole Context for render/animation threads
  updateDynamicsModes irc cDyn -- when dynamics modes change, make it so
  -- when the theme changes,
  t <- holdUniqDyn $ fmap theme cDyn -- Dynamic t String
  let t' = updated t -- Event t String
  changeTheme t'
  -- when the superDirt flag changes, make it so
  let sd = superDirt irc
  sdOn <- holdUniqDyn $ fmap superDirtOn cDyn
  performEvent_ $ fmap (liftIO . setActive sd) $ updated sdOn

updateContext :: MVar Context -> Context -> IO ()
updateContext mv x = do
  -- T.putStrLn "context updated"
  swapMVar mv x
  return ()

updateDynamicsModes :: MonadWidget t m => ImmutableRenderContext -> Dynamic t Context -> m ()
updateDynamicsModes irc ctx = do
  let nodes = mainBus irc
  dynamicsModeChanged <- liftM updated $ holdUniqDyn $ fmap dynamicsMode ctx
  performEvent_ $ fmap (liftIO . changeDynamicsMode nodes) dynamicsModeChanged


performDelayHints :: MonadWidget t m => ImmutableRenderContext -> Event t [Hint] -> m ()
performDelayHints irc hs = do
  let nodes = mainBus irc
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
