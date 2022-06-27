{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.Footer where

import Reflex
import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T
import TextShow
import Control.Monad

import qualified Estuary.Types.Term as Term
import Estuary.Types.Hint
import Estuary.Widgets.W
import Estuary.Widgets.Reflex
import Estuary.Widgets.Reflex (dynButton, invisibleButton)


footer :: MonadWidget t m => W t m ()
footer = divClass "footer code-font" $ do

  toggleTerminalButton <- divClass "footer-area" $ invisibleButton
  toggleTerminalVisible toggleTerminalButton

  sv <- statsVisible
  statsButton <- clickableDiv "footer-area" $ do
    hideableWidget' sv $ do
      cc <- fmap showt <$> clientCount
      dynText cc
      text " "
      term Term.Connections >>= dynText
      text ", "
      term Term.Latency >>= dynText
      text " "
      sl <- fmap (fmap $ (showt :: Int -> Text) . round . (*1000)) serverLatency
      dynText sl
      text "ms, "
      term Term.Load >>= dynText
      text " "
      (fmap (fmap showt) $ avgRenderLoad) >>= dynText
      text "%,   "
      (fmap (fmap showt) $ animationFPS) >>= dynText
      text "FPS ("
      (fmap (fmap (showt :: Int -> Text)) $ animationLoad) >>= dynText
      text "ms)"
  toggleStatsVisible statsButton
