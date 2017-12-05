{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Main where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Types.Hint
import Estuary.Protocol.Foreign
import Estuary.Reflex.Utility
import Estuary.Widgets.Generic
import Estuary.Widgets.Navigation
import Estuary.Widgets.PatternChain as P
import Estuary.Widgets.GeneralPattern as G -- for testing the Refactor of general container
import Estuary.Widgets.TransformedPattern
import Estuary.Widgets.Text
import Control.Monad (liftM)
import Sound.Tidal.Context (ParamPattern,Tempo(..))
import Estuary.WebDirt.Foreign
import Estuary.WebDirt.SuperDirt
import Estuary.WebDirt.Stream
import Estuary.Widgets.SpecificPattern
import Estuary.Widgets.Terminal
import Data.Map
import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Estuary.Widgets.WebSocket
import Text.JSON
import Data.Time
import Text.Read
import qualified GHCJS.Types as T

import Estuary.Types.Request
import Estuary.Types.Response


main :: IO ()
main = do
  now <- Data.Time.getCurrentTime
  let defaultTempo = Tempo {at=now,beat=0.0,cps=0.5,paused=False,clockLatency=0.2}
  tempo <- newMVar defaultTempo
  wd <- webDirt
  sd <- superDirt
  wdStream <- sampleStream wd tempo
  sdStream <- sampleStream sd tempo
  protocol <- estuaryProtocol
  mainWidget $ estuaryWidget tempo wd wdStream sd sdStream protocol now


estuaryWidget :: MonadWidget t m =>
  MVar Tempo -> WebDirt -> SampleStream -> SuperDirt -> SampleStream ->
  EstuaryProtocolObject -> UTCTime -> m ()
estuaryWidget tempo wd wdStream sd sdStream protocol now = divClass "estuary" $ mdo
  (sdOn,wdOn) <- header wsStatus clientCount
  (values,deltasUp,hints) <- divClass "page" $ navigation commands deltasDown'
  commands <- divClass "chat" $ terminalWidget deltasUp deltasDown'
  (deltasDown,wsStatus) <- alternateWebSocket protocol now deltasUp
  values' <- mapDyn (toParamPattern . StackedPatterns) values
  valuesSd <- liftM updated $ combineDyn f values' sdOn
  valuesWd <- liftM updated $ combineDyn f values' wdOn
  let deltasDown' = ffilter (not . Prelude.null) deltasDown
  clientCount <- holdDyn 0 $ fmapMaybe justServerClientCount deltasDown'
  performTempoUpdates tempo hints
  performHint wd hints
  performEvent_ $ fmap (liftIO . wdStream) valuesWd
  performEvent_ $ fmap (liftIO . sdStream) valuesSd
  where f x True = x
        f _ False = toParamPattern EmptyTransformedPattern

performTempoUpdates :: MonadWidget t m => MVar Tempo -> Event t Hint -> m ()
performTempoUpdates t h = do
  let newTempi = fmapMaybe maybeTempoHint h
  performEvent_ $ fmap (liftIO . (\x -> swapMVar t x >> return ())) newTempi

header :: (MonadWidget t m) => Dynamic t String -> Dynamic t Int -> m (Dynamic t Bool, Dynamic t Bool)
header wsStatus clientCount = divClass "header" $ do
  tick <- getPostBuild
  hostName <- performEvent $ fmap (liftIO . (\_ -> getHostName)) tick
  port <- performEvent $ fmap (liftIO . (\_ -> getPort)) tick
  hostName' <- holdDyn "" hostName
  port' <- holdDyn "" port
  divClass "logo" $ text "estuary (a TidalCycles symbiont)"
  statusMsg <- combineDyn f wsStatus clientCount
  divClass "server" $ do
    text "server: "
    dynText hostName'
    text ":"
    dynText port'
    text ": "
    dynText statusMsg
  divClass "webDirt" $ divClass "webDirtMute" $ do
      text "SuperDirt:"
      sdInput <- checkbox False $ def
      text "WebDirt:"
      wdInput <- checkbox True $ def
      return (_checkbox_value sdInput,_checkbox_value wdInput)
  where
    f "connection open" c = "(" ++ (show c) ++ " clients)"
    f x _ = x
