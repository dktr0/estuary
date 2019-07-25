{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.View where

import Reflex
import Reflex.Dom
import Data.IntMap.Strict as IntMap
import Control.Monad

import Estuary.Types.Live
import Estuary.Types.Definition
import Estuary.Types.View
import Estuary.Types.EnsembleState
import Estuary.Tidal.Types
import Estuary.Types.TextNotation
import Estuary.Types.TidalParser
import Estuary.RenderInfo
import Estuary.Widgets.EstuaryWidget
import Estuary.Types.Variable
import Estuary.Widgets.Text
import Estuary.Widgets.TransformedPattern
import Estuary.Widgets.Sequencer


viewWidget :: MonadWidget t m
  => View -> Dynamic t EnsembleState -> EstuaryWidget t m (Variable t DefinitionMap)

viewWidget (LabelView z) e = zoneWidget z "" maybeLabelText LabelText labelEditor e

viewWidget (StructureView z) e = zoneWidget z EmptyTransformedPattern maybeStructure Structure structureEditor e

viewWidget (TextView z rows) e = do
  ri <- askRenderInfo
  let errorDyn = fmap (IntMap.lookup z . errors) ri
  zoneWidget z (Live (TidalTextNotation MiniTidal,"") L3) maybeTextProgram TextProgram (textEditor rows errorDyn) e

viewWidget (SequenceView z) e = zoneWidget z defaultValue maybeSequence Sequence sequencer e
  where defaultValue = singleton 0 ("",replicate 8 False)

viewWidget (ViewDiv c v) e = liftR2 (divClass c) $ viewWidget v e

viewWidget (Views xs) e = liftM mconcat $ mapM (flip viewWidget $ e) xs

viewWidget _ _ = return mempty


zoneWidget :: MonadWidget t m
  => Int -> a -> (Definition -> Maybe a) -> (a -> Definition)
  -> (Dynamic t a -> EstuaryWidget t m (Variable t a))
  -> Dynamic t EnsembleState
  -> EstuaryWidget t m (Variable t DefinitionMap)
zoneWidget z a f g b eStateDyn = do
  let a' = fmap (IntMap.lookup z . zones) eStateDyn -- :: Dynamic t (Maybe Definition)
  let a'' = fmap (maybe Nothing f) a' -- :: Dynamic t (Maybe a)
  let a''' = fmap (maybe a id) a'' -- :: Dynamic t a
  v <- b a'''
  return $ fmap (singleton z . g) v
