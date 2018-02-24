module Estuary.Widgets.Estuary where

import Reflex
import Reflex.Dom
import Text.JSON
import Data.Time
import Text.Read
import Control.Monad.IO.Class (liftIO)

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

estuaryWidget :: MonadWidget t m
  => WebDirt -> SuperDirt -> EstuaryProtocolObject -> Context -> m ()
estuaryWidget wd sd protocol initialContext = divClass "estuary" $ mdo
  levelMeterWidget context
  headerChanges <- header wsStatus clientCount
  (values,deltasUp,hints) <- divClass "page" $ navigation (startTime initialContext) commands deltasDown'
  -- note: in preceding line (startTime initialContext) should soon become Dynamic passing of current context...
  commands <- divClass "chat" $ terminalWidget deltasUp deltasDown'
  (deltasDown,wsStatus) <- alternateWebSocket protocol now deltasUp
  p <- mapDyn (toParamPattern . StackedPatterns) values
  patternChanges <- fmap setPattern $ updated p
  renderChanges <- renderSendFlush now wd sd ctx
  levelChanges <- monitorWebDirtLevels now wd
  let deltasDown' = ffilter (not . Prelude.null) deltasDown
  let ccChange = fmap setClientCount $ fmapMaybe justServerClientCount deltasDown'
  let contextChanges = mergeWith (.) [renderChanges,patternChanges,headerChanges,levelChanges,ccChange]
  context <- foldDyn ($) initialContext contextChanges
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
  clientCount <- mapDyn clientCount ctx
  statusMsg <- combineDyn f wsStatus' clientCount'
  divClass "server" $ do
    text "server: "
    dynText hostName'
    text ":"
    dynText port'
    text ": "
    dynText statusMsg
    where
      f "connection open" c = "(" ++ (show c) ++ " clients)"
      f x _ = x
  divClass "webDirt" $ divClass "webDirtMute" $ do
    text "SuperDirt:"
    sdInput <- checkbox False $ def
    let sdOn = fmap (\x -> (\c -> c { superDirtOn = x } )) $ _checkbox_event sdInput
    text "WebDirt:"
    wdInput <- checkbox True $ def
    let wdOn = fmap (\x -> (\c -> c { webDirtOn = x } )) $ _checkbox_event wdInput
    return $ mergeWith (.) [sdOn,wdOn]
