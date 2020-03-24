{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.Footer where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Data.Text (Text)
import Data.Map.Strict
import Data.Bool
import qualified Data.Text as T
import TextShow

import Estuary.Types.Context
import Estuary.Types.RenderInfo
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Hint
import qualified Estuary.Types.Terminal as Terminal
import qualified Estuary.Types.Term as Term
import Estuary.Widgets.Terminal
import Estuary.Widgets.Generic
import Estuary.Reflex.Utility (translateDyn)


footer :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo
  -> Event t [Response] -> Event t [Hint] -> m (Event t Terminal.Command)
footer ctx renderInfo deltasDown hints = do
  let footerEvent = ffilter (elem ToggleTerminal) hints
  footerVisible <- toggle True footerEvent
  hideableWidget footerVisible "footer" $ do
    divClass "peak primary-color code-font" $ do
      dynText =<< holdUniqDyn (fmap formatServerInfo ctx)
      text ", "
      dynText =<< translateDyn Term.Load ctx
      text " "
      dynText =<< holdUniqDyn (fmap (showt . avgRenderLoad) renderInfo)
      text "%,   "
      dynText =<< holdUniqDyn (fmap (showt . animationFPS) renderInfo)
      text "FPS ("
      dynText =<< holdUniqDyn (fmap (showt . animationLoad) renderInfo)
      text "ms)"
    terminalWidget ctx deltasDown hints

formatServerInfo :: Context -> Text
formatServerInfo c = showt cc <> " connections, latency " <> showt l <> "ms"
  where
    cc = clientCount c
    l = ((round $ serverLatency c * 1000) :: Int)
