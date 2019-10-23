{-# LANGUAGE RecursiveDo, JavaScriptFFI, OverloadedStrings #-}

module Estuary.Widgets.Estuary where

import Control.Monad (liftM)

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Data.Time
import Data.Map
import Text.Read
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar
import GHCJS.Types
import GHCJS.DOM.Types (uncheckedCastTo,HTMLCanvasElement(..),HTMLDivElement(..))
import GHCJS.Marshal.Pure
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import TextShow
import Sound.MusicW.AudioContext

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
import Estuary.Types.RenderInfo
import Estuary.Render.DynamicsMode
import Estuary.Widgets.Header
import Estuary.Widgets.Footer
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble

estuaryWidget :: MonadWidget t m => ImmutableRenderContext -> MVar Context -> MVar RenderInfo -> m ()
estuaryWidget irc ctxM riM = divClass "estuary" $ mdo

  canvasWidget ctxM -- global canvas shared with render threads through Context MVar, this needs to be first in this action
  iCtx <- liftIO $ readMVar ctxM
  ctx <- foldDyn ($) iCtx contextChange -- dynamic context; near the top here so it is available for everything else
  performContext irc ctxM ctx -- perform all IO actions consequent to Context changing
  renderInfo <- pollRenderInfo riM -- dynamic render info (written by render threads, read by widgets)
  (deltasDown',wsCtxChange) <- estuaryWebSocket ctx renderInfo requestsUp
  let deltasDown = fmap (:[]) deltasDown' -- temporary hack

  let ensembleCDyn = fmap ensembleC ctx

  -- three GUI components: header, main (navigation), footer
  headerChange <- header ctx
  (requests, ensembleRequestFromPage, hintsFromPage) <- divClass "page " $ navigation ctx renderInfo deltasDown
  command <- footer ctx renderInfo deltasDown hints
  let commandRequests = attachWithMaybe commandToRequest (current ensembleCDyn) command
  let ensembleRequests = leftmost [commandRequests, ensembleRequestFromPage]

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
  let hints = mergeWith (++) [hintsFromPage, fmap (:[]) commandHint] -- Event t [Hint]
  performHints (webDirt irc) hints
  performDelayHints irc hints

  -- requests up to server
  let ensembleRequestsUp = gate (current $ fmap (inAnEnsemble . ensembleC) ctx) $ fmap EnsembleRequest ensembleRequests
  let ensembleRequestsUp' = fmap (:[]) ensembleRequestsUp
  let requests' = fmap (:[]) $ requests
  let requestsUp = mergeWith (++) [ensembleRequestsUp',requests']
  return ()

-- a standard canvas that, in addition to being part of reflex-dom's DOM representation, is shared with other threads through the Context
canvasWidget :: MonadWidget t m => MVar Context -> m ()
canvasWidget ctxM = do
  ic0 <- liftIO $ takeMVar ctxM
  let divAttrs = fromList [("class","canvas-or-svg-display"),("style",T.pack $ "z-index: -2;"), ("width","1920"), ("height","1080")]
  videoDiv <- liftM (uncheckedCastTo HTMLDivElement .  _element_raw . fst) $ elAttr' "div" divAttrs $ return ()
  let canvasAttrs = fromList [("class","canvas-or-svg-display"),("style",T.pack $ "z-index: -1;"), ("width","1920"), ("height","1080")]
  canvas <- liftM (uncheckedCastTo HTMLCanvasElement .  _element_raw . fst) $ elAttr' "canvas" canvasAttrs $ return ()
  let ic = ic0 { canvasElement = Just canvas, videoDivElement = Just videoDiv }
  liftIO $ putMVar ctxM ic


-- every .204 seconds, read the RenderInfo MVar to get load and audio level information back from the rendering/animation threads
pollRenderInfo :: MonadWidget t m => MVar RenderInfo -> m (Dynamic t RenderInfo)
pollRenderInfo riM = do
  now <- liftIO $ getCurrentTime
  riInitial <- liftIO $ readMVar riM
  ticks <- tickLossy (0.204::NominalDiffTime) now
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


<<<<<<< HEAD

ourPopUp :: MonadWidget t m => Event t () -> m (Event t ContextChange)
ourPopUp go = mdo
  let x = fmap (const True) go -- Event t Bool
  let y = fmap (const False) hide -- Event t Bool
  let xy = leftmost [x,y] -- Event t Bool
  visible <- holdDyn False xy -- Dynamic t Bool
  let visibleStyle = fmap popupVisibilityStyle visible
  let classStyle = constDyn $ singleton "class" "ourPopUp"
  let attrs = zipDynWith (union) visibleStyle classStyle
  (hide,canvasEnabledEv) <- elDynAttr "div" attrs $ do
    text "This is our popup."
    hide' <- button "hide"
    let configCheckboxAttrs = def & attributes .~ constDyn ("class" =: "config-checkbox")
    canvasEnabledEv <- divClass "config-entry" $ do
      text "Canvas:"
      canvasInput <- checkbox True configCheckboxAttrs
      return $ fmap (\x -> \c -> c { canvasOn = x }) $ _checkbox_change canvasInput
    return (hide',canvasEnabledEv)
  return canvasEnabledEv

popupVisibilityStyle :: Bool -> Map T.Text T.Text
popupVisibilityStyle False = singleton "style" "display: none"
popupVisibilityStyle True = singleton "style" "display: block"

updateContext :: MonadWidget t m => MVar Context -> Dynamic t Context -> m ()
updateContext cMvar cDyn = performEvent_ $ fmap (liftIO . void . swapMVar cMvar) $ updated cDyn
=======
performDelayHints :: MonadWidget t m => ImmutableRenderContext -> Event t [Hint] -> m ()
performDelayHints irc hs = do
  let nodes = mainBus irc
  let newDelayTime = fmapMaybe justGlobalDelayTime hs
  performEvent_ $ fmap (liftIO . changeDelay nodes) newDelayTime
  where
    f (SetGlobalDelayTime x) = Just x
    f _ = Nothing
>>>>>>> master


changeTheme :: MonadWidget t m => Event t Text -> m ()
changeTheme newStyle = performEvent_ $ fmap (liftIO . js_setThemeHref) newStyle

foreign import javascript safe
  "document.getElementById('estuary-current-theme').setAttribute('href', $1);"
<<<<<<< HEAD
  js_setThemeHref :: JSVal -> IO ()

updateDynamicsModes :: MonadWidget t m => Dynamic t Context -> m ()
updateDynamicsModes ctx = do
  iCtx <- (sample . current) ctx
  let nodes = mainBus iCtx
  dynamicsModeChanged <- liftM (updated . nubDyn) $ mapDyn dynamicsMode ctx
  performEvent_ $ fmap (liftIO . changeDynamicsMode nodes) dynamicsModeChanged

header :: (MonadWidget t m) => Dynamic t Context -> Dynamic t RenderInfo -> m (Event t ContextChange)
header ctx renderInfo = divClass "header" $ do
  clientConfigurationWidgets ctx

clientConfigurationWidgets :: (MonadWidget t m) => Dynamic t Context -> m (Event t ContextChange)
clientConfigurationWidgets ctx = divClass "config-toolbar" $ do
  themeChangeEv <- divClass "config-entry" $ do
    let styleMap =  fromList [("../css-custom/classic.css", "Classic"),("../css-custom/inverse.css","Inverse"), ("../css-custom/grayscale.css","Grayscale")]
    translateDyn Term.Theme ctx >>= dynText
    styleChange <- _dropdown_change <$> dropdown "../css-custom/classic.css" (constDyn styleMap) (def & attributes .~ constDyn ("class" =: "config-dropdown")) -- Event t String
    return $ fmap (\x c -> c {theme = x}) styleChange -- Event t (Context -> Context)

  langChangeEv <- divClass "config-entry" $ do
    translateDyn Term.Language ctx >>= dynText
    let langMap = fromList $ zip languages (fmap (T.pack . show) languages)
    langChange <- _dropdown_change <$> dropdown English (constDyn langMap) (def & attributes .~ constDyn ("class" =: "config-dropdown"))
    return $ fmap (\x c -> c { language = x }) langChange

  let condigCheckboxAttrs = def & attributes .~ constDyn ("class" =: "config-checkbox")

  superDirtEnabledEv <- divClass "config-entry" $ do
    text "SuperDirt:"
    sdInput <- checkbox False condigCheckboxAttrs
    return $ fmap (\x -> (\c -> c { superDirtOn = x } )) $ _checkbox_change sdInput

  webDirtEnabledEv <- divClass "config-entry" $ do
    text "WebDirt:"
    wdInput <- checkbox True condigCheckboxAttrs
    return $ fmap (\x -> (\c -> c { webDirtOn = x } )) $ _checkbox_change wdInput

  dynamicsModeEv <- divClass "config-entry" $ do
    text "Dynamics:"
    let dmMap = fromList $ zip dynamicsModes (fmap (T.pack . show) dynamicsModes)
    dmChange <- _dropdown_change <$> dropdown DefaultDynamics (constDyn dmMap) (def & attributes .~ constDyn ("class" =: "config-dropdown"))
    return $ fmap (\x c -> c { dynamicsMode = x }) dmChange

  popupButton <- button "popup" -- popupButton :: Event t ()
  popupContextChanges <- ourPopUp popupButton


  return $ mergeWith (.) [themeChangeEv, langChangeEv, popupContextChanges, superDirtEnabledEv, webDirtEnabledEv, dynamicsModeEv]

footer :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo
  -> Event t Request -> Event t [Response] -> Event t Hint -> m (Event t Terminal.Command)
footer ctx renderInfo deltasDown deltasUp hints = divClass "footer" $ do
  divClass "peak" $ do
    dynText . nubDyn =<< mapDyn (T.pack . f) ctx
    text " "
    dynText =<< translateDyn Term.Load ctx
    text ": "
    dynText . nubDyn =<< mapDyn (T.pack . show . avgRenderLoad) renderInfo
    text "% ("
    dynText . nubDyn =<< mapDyn (T.pack . show . peakRenderLoad) renderInfo
    text "% "
    dynText =<< translateDyn Term.Peak ctx
    text ") "
  terminalWidget ctx deltasDown deltasUp hints
  where
    f c | wsStatus c == "connection open" = "(" ++ show (clientCount c) ++ " connections, latency " ++ show (serverLatency c) ++ ")"
    f c | otherwise = "(" ++ wsStatus c ++ ")"
=======
  js_setThemeHref :: Text -> IO ()
>>>>>>> master
