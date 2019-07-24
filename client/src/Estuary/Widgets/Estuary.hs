{-# LANGUAGE RecursiveDo, JavaScriptFFI, OverloadedStrings #-}

module Estuary.Widgets.Estuary where

import Control.Monad (liftM)

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Text.JSON
import Data.Time
import Data.Map
import Text.Read
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar
import GHCJS.Types
import GHCJS.DOM.Types (uncheckedCastTo,HTMLCanvasElement(..))
import GHCJS.Marshal.Pure
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.Protocol.Foreign
import Estuary.Widgets.Navigation
import Estuary.WebDirt.SampleEngine
import Estuary.WebDirt.WebDirt
import Estuary.WebDirt.SuperDirt
import Estuary.Widgets.WebSocket
import Estuary.Types.Definition
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Context
import Estuary.Types.Hint
import Estuary.Types.Samples
import Estuary.Types.Tempo
import Estuary.Reflex.Utility
import Estuary.RenderInfo
import Estuary.Render.DynamicsMode
import Estuary.Widgets.Header
import Estuary.Widgets.Footer
import Estuary.Types.EnsembleState

estuaryWidget :: MonadWidget t m => MVar Context -> MVar RenderInfo -> m ()
estuaryWidget ctxM riM = divClass "estuary" $ mdo

  --
  iCtx <- liftIO $ readMVar ctxM
  ctx <- foldDyn ($) iCtx contextChanges -- dynamic context; is first here so available for everything else
  let ensembleStateDyn = fmap ensembleState ctx
  canvasWidget ctxM -- global canvas shared with render threads through MVar
  performContext ctxM ctx -- perform all IO actions consequent to Context changing
  renderInfo <- pollRenderInfo riM -- dynamic render info (written by render threads, read by widgets)
  samplesLoadedEv <- loadSampleMap
  (deltasDown,wsCtxChanges) <- alternateWebSocket ctx renderInfo requestsUp

  -- responses down from server
  let ensembleResponses = fmap justEnsembleResponses deltasDown
  let ensembleResponseChanges = fmap ((Prelude.foldl (.) id) . fmap responsesToStateChanges) ensembleResponses
  let ccChanges = fmap (setClientCount . fst) $ fmapMaybe justServerInfo deltasDown

  -- three GUI components: header, main (navigation), footer
  headerChanges <- header ctx
  (values, requestsFromMain, hintsFromMain, tempoChanges) <- divClass "page " $ do
    navigation ctx renderInfo deltasDown
  commands <- footer ctx renderInfo deltasDown hints

  -- hints
  let commandHints = attachPromptlyDynWithMaybe commandToHint ensembleStateDyn commands
  let hints = leftmost [hintsFromMain, commandHints]
  performHint (webDirt iCtx) hints

  -- changes to EnsembleState within Context, and to Context
  let commandChanges = fmap commandsToStateChanges commands -- commands to change of EnsembleState
  let ensembleRequests = never -- *** WARNING: BROKEN
  let requestChanges = fmap requestsToStateChanges ensembleRequests
  let tempoChanges' = fmap (\t x -> x { tempo = t }) tempoChanges
  let ensembleChanges = fmap modifyEnsemble $ mergeWith (.) [commandChanges,ensembleResponseChanges,requestChanges,tempoChanges']
  let contextChanges = mergeWith (.) [ensembleChanges, headerChanges, ccChanges, samplesLoadedEv, wsCtxChanges]

  -- requests up to server
  let commandRequests = attachPromptlyDynWithMaybe commandsToRequests ensembleStateDyn commands
  let requestsUp = fmap EnsembleRequest $ leftmost [commandRequests, ensembleRequests]
  return ()

-- a standard canvas that, in addition to being part of reflex-dom's DOM representation, is shared with other threads through the Context
canvasWidget :: MonadWidget t m => MVar Context -> m ()
canvasWidget ctxM = do
  ic0 <- liftIO $ takeMVar ctxM
  let canvasAttrs = fromList [("class","canvas-or-svg-display"),("style",T.pack $ "z-index: -1;"), ("width","1920"), ("height","1080")]
  canvas <- liftM (uncheckedCastTo HTMLCanvasElement .  _element_raw . fst) $ elAttr' "canvas" canvasAttrs $ return ()
  let ic = ic0 { canvasElement = Just canvas }
  liftIO $ putMVar ctxM ic


-- every .204 seconds, read the RenderInfo MVar to get load and audio level information back from the rendering/animation threads
pollRenderInfo :: MonadWidget t m => MVar RenderInfo -> m (Dynamic t RenderInfo)
pollRenderInfo riM = do
  now <- liftIO $ getCurrentTime
  riInitial <- liftIO $ readMVar riM
  ticks <- tickLossy (0.204::NominalDiffTime) now
  newInfo <- performEvent $ fmap (liftIO . const (readMVar riM)) ticks
  holdDyn riInitial newInfo


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


-- whenever the Dynamic representation of the Context changes, translate that
-- into various
performContext :: MonadWidget t m => MVar Context -> Dynamic t Context -> m ()
performContext cMvar cDyn = do
  iCtx <- sample $ current cDyn
  performEvent_ $ fmap (liftIO . void . swapMVar cMvar) $ updated cDyn -- transfer whole Context for render/animation threads
  updateDynamicsModes cDyn -- when dynamics modes change, make it so
  -- when the theme changes,
  t <- holdUniqDyn $ fmap theme cDyn -- Dynamic t String
  let t' = updated t -- Event t String
  changeTheme t'
  -- when the superDirt flag changes, make it so
  let sd = superDirt iCtx
  sdOn <- holdUniqDyn $ fmap superDirtOn cDyn
  performEvent_ $ fmap (liftIO . setActive sd) $ updated sdOn


updateDynamicsModes :: MonadWidget t m => Dynamic t Context -> m ()
updateDynamicsModes ctx = do
  iCtx <- (sample . current) ctx
  let nodes = mainBus iCtx
  dynamicsModeChanged <- liftM updated $ holdUniqDyn $ fmap dynamicsMode ctx
  performEvent_ $ fmap (liftIO . changeDynamicsMode nodes) dynamicsModeChanged


changeTheme :: MonadWidget t m => Event t Text -> m ()
changeTheme newStyle = performEvent_ $ fmap (liftIO . js_setThemeHref) newStyle

foreign import javascript safe
  "document.getElementById('estuary-current-theme').setAttribute('href', $1);"
  js_setThemeHref :: Text -> IO ()
