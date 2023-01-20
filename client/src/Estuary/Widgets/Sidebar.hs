{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.Sidebar where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)

import Estuary.Widgets.W
import Estuary.Widgets.Reflex
import Estuary.Widgets.Reference
import Estuary.Widgets.Config
import Estuary.Types.Term
import Estuary.Types.RenderInfo

sidebarWidget :: MonadWidget t m => W t m ()
sidebarWidget = do
  referenceEvent <- fmap (1 <$) $ clickableDiv "sidebar-tab" $ (dynText =<< term Reference)
  configEvent <- fmap (2 <$) $ clickableDiv "sidebar-tab" $ (dynText =<< term Settings)
  curPage <- holdDyn 1 $ leftmost [referenceEvent,configEvent]
  hideableWidget (fmap (==1) curPage) "reference" $ navigation   -- Reference panel
  hideableWidget (fmap (==2) curPage) "config-options" configWidget
