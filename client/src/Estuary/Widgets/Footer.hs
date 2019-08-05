{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.Footer where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
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
import Estuary.Reflex.Utility (translateDyn)


footer :: MonadWidget t m => Dynamic t Context -> Dynamic t RenderInfo
  -> Event t [Response] -> Event t [Hint] -> m (Event t Terminal.Command)
footer ctx renderInfo deltasDown hints = divClass "footer" $ do
  divClass "peak primary-color code-font" $ do
    serverInfo <- holdUniqDyn $ fmap (\c -> (clientCount c,serverLatency c)) ctx
    dynText $ fmap f serverInfo
    text " "
    dynText =<< translateDyn Term.Load ctx
    text ": "
    dynText =<< holdUniqDyn (fmap (T.pack . show . avgRenderLoad) renderInfo)
    text "%"
  terminalWidget ctx deltasDown hints
  where
    f (cc,lat) = "(" <> (showt cc) <> " connections, latency " <> (showt $ (realToFrac lat :: Double)) <> ")"
