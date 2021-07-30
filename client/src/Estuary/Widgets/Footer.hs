{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Footer where

import Reflex
import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T
import TextShow
import Control.Monad

import Estuary.Types.Context
import Estuary.Types.RenderInfo
import qualified Estuary.Types.Term as Term
import Estuary.Types.Hint
import Estuary.Widgets.W
import Estuary.Widgets.Reflex
import Estuary.Widgets.Reflex (dynButton, invisibleButton)

footer :: MonadWidget t m => Event t [Hint] -> W t m (Event t ())
footer hints = divClass "footer code-font" $ mdo
  toggleTerminalButton <- divClass "footer-area" $ invisibleButton
  let statsShortcut = ffilter (elem ToggleStats) hints
  let statsEvent = leftmost [() <$ statsShortcut, statsButton]
  statsVisible <- toggle True statsEvent
  statsButton <- clickableDiv "footer-area" $ do
    hideableWidget' statsVisible $ do
      ctx <- context
      ri <- renderInfo
      cc <- fmap (fmap showt) $ holdUniqDyn $ fmap clientCount ctx
      dynText cc
      text " "
      term Term.Connections >>= dynText
      text ", "
      term Term.Latency >>= dynText
      text " "
      sl <- fmap (fmap $ (showt :: Int -> Text) . round . (*1000)) $ holdUniqDyn $ fmap serverLatency ctx
      dynText sl
      text "ms, "
      term Term.Load >>= dynText
      text " "
      (fmap (fmap showt) $ holdUniqDyn $ fmap avgRenderLoad ri) >>= dynText
      text "%,   "
      (fmap (fmap showt) $ holdUniqDyn $ fmap animationFPS ri) >>= dynText
      text "FPS ("
      (fmap (fmap (showt :: Int -> Text)) $ holdUniqDyn $ fmap animationLoad ri) >>= dynText
      text "ms)"
  return toggleTerminalButton
