{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.View where

import Reflex
import Reflex.Dom
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import TextShow
import Data.Time

import Estuary.Types.Live
import Estuary.Types.Definition
import Estuary.Types.View
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Types.Context
import Estuary.Tidal.Types
import Estuary.Types.TextNotation
import Estuary.Types.TidalParser
import Estuary.Types.RenderInfo
import Estuary.Widgets.Editor
import Estuary.Widgets.Generic
import Estuary.Types.Variable
import Estuary.Widgets.Text
import Estuary.Widgets.TransformedPattern
import Estuary.Widgets.Sequencer
import Estuary.Widgets.Roulette
import Estuary.Widgets.EnsembleStatus
import Estuary.Widgets.Tempo
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.Hint
import Estuary.Widgets.AudioMap
import Estuary.Widgets.StopWatch

viewWidget :: MonadWidget t m => Event t [EnsembleResponse] -> View -> Editor t m (Event t EnsembleRequest)

viewWidget er EmptyView = return never

viewWidget er (LabelView z) = zoneWidget z "" maybeLabelText LabelText er labelEditor


viewWidget er (StructureView z) = zoneWidget z EmptyTransformedPattern maybeTidalStructure TidalStructure er structureEditor

viewWidget er (TextView z rows) = do
  whenever <- liftIO $ getCurrentTime
  ri <- renderInfo
  let errorDyn = fmap (IntMap.lookup z . errors) ri
  zoneWidget z (Live (TidalTextNotation MiniTidal,"",whenever) L3) maybeTextProgram TextProgram er (textProgramEditor rows errorDyn)

viewWidget er (SequenceView z) = zoneWidget z defaultValue maybeSequence Sequence er sequencer
  where defaultValue = Map.singleton 0 ("",replicate 8 False)

viewWidget er EnsembleStatusView = ensembleStatusWidget

viewWidget er (RouletteView z rows wrappingBool) = zoneWidget z [] maybeRoulette Roulette er (rouletteWidget rows wrappingBool)

viewWidget er (StopWatchView z) = zoneWidget z (Left Nothing) maybeStopWatch StopWatch er stopWatchWidget

viewWidget er TempoView = do
  ctx <- context
  iCtx <- sample $ current ctx
  let initialTempo = (tempo . ensemble . ensembleC) iCtx
  tempoDelta <- holdDyn initialTempo $ fmapMaybe lastTempoChange er
  tempoE <- tempoWidget tempoDelta
  return $ fmap WriteTempo tempoE

viewWidget er (Paragraph t) = divClass "paragraph code-font" $ translatableText t >> return never

viewWidget er (Example n t) = do
  b <- clickableDiv "example code-font" $ text t
  bTime <- performEvent $ fmap (liftIO . const getCurrentTime) b
  hint $ fmap (\et -> ZoneHint 1 (TextProgram (Live (n,t,et) L3))) bTime
  return never

viewWidget er (ViewDiv c v) = divClass c $ viewWidget er v

viewWidget er (BorderDiv v) = divClass "borderDiv" $ viewWidget er v

viewWidget er (Views xs) = divClass "views" $ liftM leftmost $ mapM (viewWidget er) xs

viewWidget er (GridView c r vs) = viewsContainer $ liftM leftmost $ mapM (\v -> divClass "gridChild" $ viewWidget er v) vs
  where
    viewsContainer x = elAttr "div" ("class" =: "gridView" <> "style" =: (setColumnsAndRows) ) $ x
    defineNumRowsOrColumns n = replicate n $ showt ((100.0 :: Double) / (fromIntegral n)) <> "%"
    setNumColumns =  "grid-template-columns: " <> (T.intercalate " " $ defineNumRowsOrColumns c) <> ";"
    setNumRows =  "grid-template-rows: " <> (T.intercalate " " $ defineNumRowsOrColumns r) <> ";"
    setColumnsAndRows  = setNumColumns <> setNumRows

viewWidget _ AudioMapView = do
  audioMapWidget
  return never

zoneWidget :: (MonadWidget t m, Eq a)
  => Int -> a -> (Definition -> Maybe a) -> (a -> Definition) -> Event t [EnsembleResponse]
  -> (Dynamic t a -> Editor t m (Variable t a))
  -> Editor t m (Event t EnsembleRequest)
zoneWidget z defaultA f g ensResponses anEditorWidget = do
  ctx <- context
  iCtx <- sample $ current ctx
  let iDef = IntMap.findWithDefault (g defaultA) z $ zones $ ensemble $ ensembleC iCtx
  let iValue = maybe defaultA id $ f iDef
  let deltas = fmapMaybe (join . fmap f . listToMaybe . reverse . justEditsInZone z) ensResponses -- :: Event t a
  dynUpdates <- holdDyn iValue deltas
  variableFromWidget <- anEditorWidget dynUpdates
  return $ (WriteZone z . g) <$> localEdits variableFromWidget
