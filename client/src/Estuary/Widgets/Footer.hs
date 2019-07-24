{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.Footer where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import qualified Data.Text as T

import Estuary.Types.Context
import Estuary.RenderInfo
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Hint
import qualified Estuary.Types.Terminal as Terminal
import qualified Estuary.Types.Term as Term
import Estuary.Widgets.Terminal
import Estuary.Reflex.Utility (translateDyn)


footer :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo
  -> Event t [Response] -> Event t Hint -> m (Event t Terminal.Command)
footer ctx renderInfo deltasDown hints = divClass "footer" $ do
  divClass "peak primary-color code-font" $ do
    dynText =<< holdUniqDyn (fmap f ctx)
    text " "
    dynText =<< translateDyn Term.Load ctx
    text ": "
    dynText =<< holdUniqDyn (fmap (T.pack . show . avgRenderLoad) renderInfo)
    text "% ("
    dynText =<< holdUniqDyn (fmap (T.pack . show . peakRenderLoad) renderInfo)
    text "% "
    dynText =<< translateDyn Term.Peak ctx
    text ") "
  terminalWidget ctx deltasDown hints
  where
    f c | wsStatus c == "connection open" = "(" <> (T.pack $ show $ clientCount c) <> " connections, latency " <> (T.pack $ show $ serverLatency c) <> ")"
    f c | otherwise= "(" <> wsStatus c <> ")"
