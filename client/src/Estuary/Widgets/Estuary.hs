{-# LANGUAGE RecursiveDo, JavaScriptFFI #-}

module Estuary.Widgets.Estuary where

import Control.Monad (liftM)

import Reflex
import Reflex.Dom
import Text.JSON
import Data.Time
import Data.Map
import Text.Read
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar
import GHCJS.Types
import GHCJS.Marshal.Pure
import Data.Functor (void)

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
import Estuary.Widgets.LevelMeters
import Estuary.Widgets.Terminal
import Estuary.Reflex.Utility
import Estuary.Types.Language
import Estuary.Help.LanguageHelp
import Estuary.Languages.TidalParsers
import qualified Estuary.Types.Term as Term
import Estuary.RenderInfo
import qualified Estuary.Types.Terminal as Terminal

estuaryWidget :: MonadWidget t m => Navigation -> MVar Context -> MVar RenderInfo -> EstuaryProtocolObject -> m ()
estuaryWidget initialPage ctxM riM protocol = divClass "estuary" $ mdo
  ic <- liftIO $ readMVar ctxM
  renderInfo <- pollRenderInfoChanges riM

  -- load the samples map, if there is a better way to triiger an event from an async callback
  -- then this should be update to reflect that.
  postBuild <- getPostBuild
  samplesLoadedEv <- performEventAsync $ ffor postBuild $ \_ triggerEv -> liftIO $ do
    loadSampleMapAsync defaultSampleMapURL $ \maybeMap -> do
      case maybeMap of
        Nothing -> return () -- Couldn't load the map
        Just map -> triggerEv $ setSampleMap map

  (headerChanges, clickedLogoEv) <- header ctx renderInfo

  (values, deltasUp, hints, tempoChanges) <- divClass "page" $ do
    navigation initialPage (Splash <$ clickedLogoEv) ctx renderInfo commands deltasDown

  commands <- footer ctx renderInfo deltasUp deltasDown' hints

  (deltasDown,wsStatus) <- alternateWebSocket protocol deltasUp
  let definitionChanges = fmapMaybe (fmap setDefinitions) $ updated values
  let deltasDown' = ffilter (not . Prelude.null) deltasDown
  let wsChange = fmap (\w x -> x { wsStatus = w }) $ (updated . nubDyn) wsStatus
  let ccChange = fmap setClientCount $ fmapMaybe justServerClientCount deltasDown'
  let tempoChanges' = fmap (\t x -> x { tempo = t }) tempoChanges
  let contextChanges = mergeWith (.) [definitionChanges, headerChanges, ccChange, tempoChanges', samplesLoadedEv, wsChange]
  ctx <- foldDyn ($) ic contextChanges -- Dynamic t Context

  t <- mapDyn theme ctx -- Dynamic t String
  let t' = updated t -- Event t String
  changeTheme t'

  updateContext ctxM ctx

  performHint (webDirt ic) hints

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

header :: (MonadWidget t m) => Dynamic t Context -> Dynamic t RenderInfo -> m (Event t ContextChange, Event t ())
header ctx renderInfo = divClass "header" $ do
  tick <- getPostBuild
  hostName <- performEvent $ fmap (liftIO . (\_ -> getHostName)) tick
  port <- performEvent $ fmap (liftIO . (\_ -> getPort)) tick
  hostName' <- holdDyn "" hostName
  port' <- holdDyn "" port
  clickedLogoEv <- dynButtonWithChild "logo" $
    dynText =<< translateDyn Term.EstuaryDescription ctx
  ctxChangeEv <- clientConfigurationWidgets ctx
  return (ctxChangeEv, clickedLogoEv)

clientConfigurationWidgets :: (MonadWidget t m) => Dynamic t Context -> m (Event t ContextChange)
clientConfigurationWidgets ctx = divClass "webDirt" $ do
  divClass "webDirtMute" $ divClass "webDirtContent" $ do
    let styleMap =  fromList [("../css-custom/classic.css", "Classic"),("../css-custom/inverse.css","Inverse"), ("../css-custom/grayscale.css","Grayscale")]
    translateDyn Term.Theme ctx >>= dynText
    styleChange <- divClass "themeSelector" $ do _dropdown_change <$> dropdown "../css-custom/classic.css" (constDyn styleMap) def -- Event t String
    let styleChange' = fmap (\x c -> c {theme = x}) styleChange -- Event t (Context -> Context)
    translateDyn Term.Language ctx >>= dynText
    let langMap = constDyn $ fromList $ zip languages (fmap show languages)
    langChange <- divClass "languageSelector" $ do _dropdown_change <$> (dropdown English langMap def)
    let langChange' = fmap (\x c -> c { language = x }) langChange
    text "SuperDirt:"
    sdInput <- divClass "superDirtCheckbox" $ checkbox False $ def
    let sdOn = fmap (\x -> (\c -> c { superDirtOn = x } )) $ _checkbox_change sdInput
    text "WebDirt:"
    wdInput <-divClass "webDirtCheckbox" $ checkbox True $ def
    let wdOn = fmap (\x -> (\c -> c { webDirtOn = x } )) $ _checkbox_change wdInput
    return $ mergeWith (.) [langChange',sdOn,wdOn, styleChange']

footer :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo
  -> Event t Request -> Event t [Response] -> Event t Hint -> m (Event t Terminal.Command)
footer ctx renderInfo deltasDown deltasUp hints = divClass "footer" $ do
  divClass "peak" $ do
    text "server "
    wsStatus' <- mapDyn wsStatus ctx
    clientCount' <- mapDyn clientCount ctx
    dynText =<< combineDyn f wsStatus' clientCount'
    text " "
    dynText =<< translateDyn Term.Load ctx
    text ": "
    dynText =<< mapDyn (show . avgRenderLoad) renderInfo
    text "% ("
    dynText =<< mapDyn (show . peakRenderLoad) renderInfo
    text "% "
    dynText =<< translateDyn Term.Peak ctx
    text ") "
  terminalWidget ctx deltasDown deltasUp hints
  where
    f "connection open" c = "(" ++ (show c) ++ " connections)"
    f x _ = "(" ++ x ++ ")"
