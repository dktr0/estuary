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
import Estuary.RenderInfo
import Estuary.Types.Tempo
import Estuary.Widgets.View
import Estuary.Widgets.Tempo
import Estuary.Types.Hint
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Types.View
import Estuary.Types.Variable
import Estuary.Widgets.EstuaryWidget
import Estuary.Types.EnsembleRequest


ensembleView :: MonadWidget t m
  => EstuaryWidget t m (Event t [EnsembleRequest])
ensembleView = do

  ctx <- askContext

  -- Ensemble name and username UI (created only if ensemble is not "", ie. not in solo mode)
  iCtx <- reflex $ sample $ current ctx
  let eName = ensembleName $ ensemble $ ensembleC iCtx
  reflex $ when (eName /= "") $ divClass "ensembleHeader primary-color ui-font" $ do
    let uName = userHandle $ ensembleC iCtx
    divClass "ensembleName ui-font primary-color" $ text $ "Ensemble: " <> eName
    divClass "ensembleHandle ui-font primary-color" $ text $ "UserName: " <> uName

  -- Tempo UI
  tempoE <- tempoWidget
  let tempoRequests = fmap ( (:[]) . WriteTempo) tempoE

  -- Dynamic core View UI
  currentView <- reflex $ holdUniqDyn $ fmap (activeView . ensembleC) ctx
  let dynamicViews = fmap viewWidget currentView -- Dynamic t (EstuaryWidget t m (Event t [EnsembleRequest]))
  x <- dynEstuaryWidget dynamicViews -- Dynamic t (Event t [EnsembleRequest])
  let widgetRequests = switchPromptlyDyn x -- Event t [EnsembleRequest]

  return $ mergeWith (++) [tempoRequests,widgetRequests]
