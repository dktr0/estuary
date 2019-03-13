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
import qualified Data.Text as T

import Estuary.Tidal.Types
import Estuary.Protocol.Foreign
import Estuary.Widgets.Navigation
import Estuary.WebDirt.SampleEngine
import Estuary.WebDirt.WebDirt
import Estuary.WebDirt.SuperDirt
import Estuary.Widgets.WebSocket
import Estuary.Types.Definition
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Terminal
import Estuary.Types.Context
import Estuary.Types.Hint
import Estuary.Types.Samples
import Estuary.Types.Tempo
import Estuary.Widgets.Terminal
import Estuary.Reflex.Utility
import Estuary.Types.Language
import Estuary.Help.LanguageHelp
import Estuary.Languages.TidalParsers
import qualified Estuary.Types.Term as Term
import Estuary.RenderInfo
import Estuary.Render.DynamicsMode
import qualified Estuary.Types.Terminal as Terminal

estuaryWidget :: MonadWidget t m => Navigation -> MVar Context -> MVar RenderInfo -> EstuaryProtocolObject -> m ()
estuaryWidget initialPage ctxM riM protocol = divClass "estuary" $ do

  ic0 <- liftIO $ takeMVar ctxM
  let canvasAttrs = fromList [("class","canvas"),("style",T.pack $ "z-index: -1;"), ("width","1920"), ("height","1080")]
  canvas <- liftM (uncheckedCastTo HTMLCanvasElement .  _element_raw . fst) $ elAttr' "canvas" canvasAttrs $ return ()
  let ic = ic0 { canvasElement = Just canvas }
  liftIO $ putMVar ctxM ic

  renderInfo <- pollRenderInfoChanges riM



  -- load the samples map, if there is a better way to trigger an event from an async callback
  -- then this should be update to reflect that.
  postBuild <- getPostBuild
  samplesLoadedEv <- performEventAsync $ ffor postBuild $ \_ triggerEv -> liftIO $ do
    loadSampleMapAsync defaultSampleMapURL $ \maybeMap -> do
      case maybeMap of
        Nothing -> return () -- Couldn't load the map
        Just map -> triggerEv $ setSampleMap map

  (ctx, hints) <- mdo
    -- ctx's foldDyn creation must come before the structure widgets below incase
    -- any inspect the `current` value. This will block indefinitly until it has a
    -- chance to be created.
    let definitionChanges = fmapMaybe (fmap setDefinitions) $ updated values
    let deltasDown' = ffilter (not . Prelude.null) deltasDown
    let ccChange = fmap setClientCount $ fmapMaybe justServerClientCount deltasDown'
    let tempoChanges' = fmap (\t x -> x { tempo = t }) tempoChanges
    let contextChanges = mergeWith (.) [definitionChanges, headerChanges, ccChange, tempoChanges', samplesLoadedEv, wsCtxChanges]
    ctx <- foldDyn ($) ic contextChanges -- Dynamic t Context

    headerChanges <- header ctx renderInfo

    (values, deltasUp, hints, tempoChanges) <- divClass "page" $ do
      navigation initialPage never ctx renderInfo commands deltasDown

    commands <- footer ctx renderInfo deltasUp deltasDown' hints

    (deltasDown,wsCtxChanges) <- alternateWebSocket protocol deltasUp

    return (ctx, hints)

  t <- nubDyn <$> mapDyn theme ctx -- Dynamic t String
  let t' = updated t -- Event t String
  changeTheme t'

  let sd = superDirt ic
  sdOn <- nubDyn <$> mapDyn superDirtOn ctx
  performEvent_ $ fmap (liftIO . setActive sd) $ updated sdOn

  updateDynamicsModes ctx

  updateContext ctxM ctx

  performHint (webDirt ic) hints


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

pollRenderInfoChanges :: MonadWidget t m => MVar RenderInfo -> m (Dynamic t RenderInfo)
pollRenderInfoChanges riM = do
  now <- liftIO $ getCurrentTime
  riInitial <- liftIO $ readMVar riM
  ticks <- tickLossy (0.204::NominalDiffTime) now
  newInfo <- performEvent $ fmap (liftIO . const (readMVar riM)) ticks
  holdDyn riInitial newInfo

changeTheme :: MonadWidget t m => Event t String -> m ()
changeTheme newStyle = performEvent_ $ fmap (liftIO . js_setThemeHref . pToJSVal) newStyle

foreign import javascript safe
  "document.getElementById('estuary-current-theme').setAttribute('href', $1);"
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
