{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.Ensemble (
  ensembleView
) where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Data.Time.Clock
import Control.Monad
import Control.Monad.Trans
import qualified Data.IntMap.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.Widgets.Reflex
import Estuary.Widgets.Editor
import Estuary.Types.Context
import Estuary.Types.Request
import Estuary.Types.Response
import Estuary.Types.Definition
import Estuary.Types.RenderInfo
import Estuary.Types.Tempo
import Estuary.Widgets.View
import Estuary.Widgets.Tempo
import Estuary.Types.Hint
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Types.View
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse


ensembleView :: MonadWidget t m
  => Event t [EnsembleResponse] -> Editor t m (Event t EnsembleRequest)
ensembleView ensResponses = do
  ctx <- context
  currentView <- holdUniqDyn $ fmap (activeView . ensembleC) ctx
  let dynamicViews = fmap (viewWidget ensResponses) currentView -- Dynamic t (Editor t m (Event t EnsembleRequest))
  x <- dyn' dynamicViews -- Dynamic t (Event t EnsembleRequest)
  let widgetRequests = switchDyn x -- Event t EnsembleRequest
  return widgetRequests
