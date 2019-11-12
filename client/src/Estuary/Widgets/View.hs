{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.View where

import Reflex
import Reflex.Dom
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Control.Monad
import Data.Maybe
import TextShow

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
import Estuary.Widgets.EnsembleStatus
import Estuary.Types.EnsembleRequest
import Estuary.Types.EnsembleResponse
import Estuary.Types.Hint

viewWidget :: MonadWidget t m => Event t [EnsembleResponse] -> View -> Editor t m (Event t EnsembleRequest)

viewWidget er EmptyView = return never

viewWidget er (LabelView z) = zoneWidget z "" maybeLabelText LabelText er labelEditor

viewWidget er (StructureView z) = zoneWidget z EmptyTransformedPattern maybeStructure Structure er structureEditor

viewWidget er (TextView z rows) = do
  ri <- askRenderInfo
  let errorDyn = fmap (IntMap.lookup z . errors) ri
  zoneWidget z (Live (TidalTextNotation MiniTidal,"") L3) maybeTextProgram TextProgram er (textProgramEditor rows errorDyn)

viewWidget er (SequenceView z) = zoneWidget z defaultValue maybeSequence Sequence er sequencer
  where defaultValue = Map.singleton 0 ("",replicate 8 False)

viewWidget er EnsembleStatusView = ensembleStatusWidget >> return never

viewWidget er (Paragraph t) = liftR2 (elClass "div" "paragraph") $ translatedText t >> return never

viewWidget er (Example n t) = do
  b <- liftR $ clickableDiv "example" $ text t
  hint $ (ZoneHint 1 (TextProgram (Live (n,t) L3))) <$ b
  return never

viewWidget er (ViewDiv c v) = liftR2 (divClass c) $ viewWidget er v

viewWidget er (BorderDiv v) = liftR2 (divClass "borderDiv") $ viewWidget er v

viewWidget er (Views xs) = liftM leftmost $ mapM (viewWidget er) xs

viewWidget er (GridView c r vs) =  liftR2 viewsContainer $ liftM leftmost $ mapM (\v -> liftR2 (divClass "gridChild") $ viewWidget er v ) vs
  where
    -- subGridDiv x = divClass "subGrid-container" $ x
    viewsContainer x = elAttr "div" ("class" =: "gridView" <> "style" =: (setColumnsAndRows)) $ x
    defineNumRowsOrColumns n = replicate n $ showt ((100.0 :: Double) / (fromIntegral n)) <> "%"
    setNumColumns =  "grid-template-columns: " <> (T.intercalate " " $ defineNumRowsOrColumns c) <> ";"
    setNumRows =  "grid-template-rows: " <> (T.intercalate " " $ defineNumRowsOrColumns r) <> ";"
    setColumnsAndRows  = setNumColumns <> setNumRows


zoneWidget :: (MonadWidget t m, Eq a)
  => Int -> a -> (Definition -> Maybe a) -> (a -> Definition) -> Event t [EnsembleResponse]
  -> (Dynamic t a -> Editor t m (Variable t a))
  -> Editor t m (Event t EnsembleRequest)
zoneWidget z defaultA f g ensResponses anEditorWidget = do
  ctx <- askContext
  iCtx <- initialValueOfDyn ctx
  let iDef = IntMap.findWithDefault (g defaultA) z $ zones $ ensemble $ ensembleC iCtx
  let iValue = maybe defaultA id $ f iDef
  let deltas = fmapMaybe (join . fmap f . listToMaybe . reverse . justEditsInZone z) ensResponses -- :: Event t a
  dynUpdates <- liftR $ holdDyn iValue deltas
  variableFromWidget <- anEditorWidget dynUpdates
  return $ (WriteZone z . g) <$> localEdits variableFromWidget
