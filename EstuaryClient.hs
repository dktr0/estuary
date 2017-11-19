{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Main where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Protocol.Foreign
import Estuary.Reflex.Utility
import Estuary.Widgets.Generic
import Estuary.Widgets.Navigation
import Estuary.Widgets.PatternChain as P
import Estuary.Widgets.GeneralPattern as G -- for testing the Refactor of general container
import Estuary.Widgets.TransformedPattern
import Estuary.Widgets.Text
import Control.Monad (liftM)
import Sound.Tidal.Context (ParamPattern)
import Estuary.WebDirt.Foreign
import Estuary.WebDirt.Stream
import Estuary.WebDirt.SuperDirt
import Estuary.Widgets.SpecificPattern
import Estuary.Widgets.Terminal
import Data.Map
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
  wd <- webDirt
  sd <- superDirt
  stream <- webDirtStream wd sd
  protocol <- estuaryProtocol
  now <- Data.Time.getCurrentTime
  mainWidget $ estuaryWidget wd stream protocol now


estuaryWidget :: MonadWidget t m => WebDirt -> WebDirtStream -> EstuaryProtocolObject -> UTCTime -> m ()
estuaryWidget wd stream protocol now = divClass "estuary" $ mdo
  muted <- header wsStatus clientCount
  (values,deltasUp,hints) <- divClass "page" $ navigation commands deltasDown'
  commands <- divClass "chat" $ terminalWidget deltasUp deltasDown'
  (deltasDown,wsStatus) <- alternateWebSocket protocol now deltasUp
  values' <- mapDyn (toParamPattern . StackedPatterns) values
  values'' <- combineDyn f values' muted
  let values''' = updated values''
  let deltasDown' = ffilter (not . Prelude.null) deltasDown
  clientCount <- holdDyn 0 $ fmapMaybe justServerClientCount deltasDown'
  -- diagnostics values deltasUp deltasDown' hints
  performHint wd hints
  performEvent_ $ fmap (liftIO . stream) values'''
  where f x False = x
        f _ True = toParamPattern EmptyTransformedPattern


header :: (MonadWidget t m) => Dynamic t String -> Dynamic t Int -> m (Dynamic t Bool)
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
  muted' <- divClass "webDirt" $ do
    muted <- divClass "webDirtMute" $ do
      text "WebDirt Mute "
      checkbox False $ def
    return $ _checkbox_value muted
  return muted'
  where
    f "connection open" c = "(" ++ (show c) ++ " clients)"
    f x _ = x
