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
import Estuary.Types.Variable
import Estuary.Widgets.Editor
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse


ensembleView :: MonadWidget t m
  => Event t [EnsembleResponse] -> Editor t m (Event t EnsembleRequest)
ensembleView ensResponses = do

  -- Ensemble name and username UI (created only if ensemble is not "", ie. not in solo mode)
  ctx <- askContext
  iCtx <- initialValueOfDyn ctx
  let eName = ensembleName $ ensemble $ ensembleC iCtx
  liftR $ when (eName /= "") $ divClass "ensembleHeader primary-color ui-font" $ do
    let uName = userHandle $ ensembleC iCtx
    divClass "ensembleName ui-font primary-color" $ text $ "Ensemble: " <> eName
    divClass "ensembleHandle ui-font primary-color" $ text $ "UserName: " <> uName

  -- Dynamic core View UI
  currentView <- liftR $ holdUniqDyn $ fmap (activeView . ensembleC) ctx
  let dynamicViews = fmap (viewWidget ensResponses) currentView -- Dynamic t (Editor t m (Event t EnsembleRequest))
  x <- dynEditor dynamicViews -- Dynamic t (Event t EnsembleRequest)
  let widgetRequests = switchDyn x -- Event t EnsembleRequest
  return widgetRequests
