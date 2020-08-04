{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.Sidebar where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)

import Estuary.Widgets.Editor
import Estuary.Widgets.Generic
import Estuary.Widgets.Reference
import Estuary.Widgets.Config
import Estuary.Types.Context
import Estuary.Types.Term

sidebarWidget :: MonadWidget t m => Editor t m (Event t ContextChange)
sidebarWidget = do
  referenceEvent <- fmap (1 <$) $ clickableDiv "sidebar-tab" $ (dynText =<< term Reference)
  configEvent <- fmap (2 <$) $ clickableDiv "sidebar-tab" $ (dynText =<< term Settings)
  curPage <- holdDyn 1 $ leftmost [referenceEvent,configEvent]
  hideableWidget (fmap (==1) curPage) "reference" $ navigation   -- Reference panel
  configChange <- hideableWidget (fmap (==2) curPage) "config-options" $ configWidget
  return configChange
