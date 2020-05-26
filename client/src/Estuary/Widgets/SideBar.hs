{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.SideBar where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Estuary.Widgets.Generic
import Estuary.Widgets.Reference
import Estuary.Widgets.Config

sidebarWidget :: MonadWidget t m => m ()
sidebarWidget = do
  referenceEvent <- fmap (1 <$) $ clickableDiv "sidebar-tab" $ text "Reference"
  configEvent <- fmap (2 <$) $ clickableDiv "sidebar-tab" $ text "Config"
  return ()
  curPage <- holdDyn 1 $ leftmost [referenceEvent,configEvent]
  hideableWidget' (fmap (==1) curPage) $ referenceWidget
  hideableWidget' (fmap (==2) curPage) $ configWidget
