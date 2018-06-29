{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.Estuary where

import Reflex
import Reflex.Dom
import Text.JSON
import Data.Time
import Text.Read
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar

import Estuary.Tidal.Types
import Estuary.Protocol.Foreign
import Estuary.Widgets.Navigation
import Estuary.WebDirt.SampleEngine
import Estuary.WebDirt.WebDirt
import Estuary.WebDirt.SuperDirt
import Estuary.Widgets.WebSocket
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Context
import Estuary.Widgets.LevelMeters
import Estuary.Widgets.Terminal

estuaryWidget :: MonadWidget t m
  => MVar Renderer -> WebDirt -> SuperDirt -> EstuaryProtocolObject -> Context -> m ()
estuaryWidget renderM wd sd protocol initialContext = divClass "estuary" $ mdo
  levelMeterWidget context
  headerChanges <- header context
  (values,deltasUp,hints) <- divClass "page" $ navigation (startTime initialContext) commands deltasDown'
  commands <- divClass "chat" $ terminalWidget deltasUp deltasDown'
  (deltasDown,wsStatus) <- alternateWebSocket protocol (startTime initialContext) deltasUp
  p <- mapDyn (toParamPattern . StackedPatterns) values
  let patternChanges = fmap setPattern $ updated p
  let deltasDown' = ffilter (not . Prelude.null) deltasDown
  let ccChange = fmap setClientCount $ fmapMaybe justServerClientCount deltasDown'
  let contextChanges = mergeWith (.) [patternChanges,headerChanges,ccChange]
  context <- foldDyn ($) initialContext contextChanges
  dynamicRender renderM wd sd context
  performHint wd hints



header :: (MonadWidget t m) => Dynamic t Context -> m (Event t ContextChange)
header ctx = divClass "header" $ do
  tick <- getPostBuild
  hostName <- performEvent $ fmap (liftIO . (\_ -> getHostName)) tick
  port <- performEvent $ fmap (liftIO . (\_ -> getPort)) tick
  hostName' <- holdDyn "" hostName
  port' <- holdDyn "" port
  divClass "logo" $ text "estuary (a TidalCycles symbiont)"
  wsStatus' <- mapDyn wsStatus ctx
  clientCount' <- mapDyn clientCount ctx
  statusMsg <- combineDyn f wsStatus' clientCount'
  divClass "server" $ do
    text "server: "
    dynText hostName'
    text ":"
    dynText port'
    text ": "
    dynText statusMsg
  webDirtSuperDirtToggles
  where
    f "connection open" c = "(" ++ (show c) ++ " clients)"
    f x _ = x

webDirtSuperDirtToggles :: (MonadWidget t m) => m (Event t ContextChange)
webDirtSuperDirtToggles = divClass "webDirt" $ divClass "webDirtMute" $ do
  text "SuperDirt:"
  sdInput <- checkbox False $ def
  let sdOn = fmap (\x -> (\c -> c { superDirtOn = x } )) $ _checkbox_change sdInput
  text "WebDirt:"
  wdInput <- checkbox True $ def
  let wdOn = fmap (\x -> (\c -> c { webDirtOn = x } )) $ _checkbox_change wdInput
  return $ mergeWith (.) [sdOn,wdOn]

