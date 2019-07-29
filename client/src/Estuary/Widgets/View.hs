{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.View where

import Reflex
import Reflex.Dom
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Control.Monad

import Estuary.Types.Live
import Estuary.Types.Definition
import Estuary.Types.View
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Types.Context
import Estuary.Tidal.Types
import Estuary.Types.TextNotation
import Estuary.Types.TidalParser
import Estuary.RenderInfo
import Estuary.Widgets.Editor
import Estuary.Types.Variable
import Estuary.Widgets.Text
import Estuary.Widgets.TransformedPattern
import Estuary.Widgets.Sequencer
import Estuary.Types.EnsembleRequest


viewWidget :: MonadWidget t m => View -> Editor t m (Event t [EnsembleRequest])

viewWidget (LabelView z) = zoneWidget z "" maybeLabelText LabelText labelEditor

viewWidget (StructureView z) = zoneWidget z EmptyTransformedPattern maybeStructure Structure structureEditor

viewWidget (TextView z rows) = do
  ri <- askRenderInfo
  let errorDyn = fmap (IntMap.lookup z . errors) ri
  zoneWidget z (Live (TidalTextNotation MiniTidal,"") L3) maybeTextProgram TextProgram (textProgramEditor rows errorDyn)

viewWidget (SequenceView z) = zoneWidget z defaultValue maybeSequence Sequence sequencer
  where defaultValue = Map.singleton 0 ("",replicate 8 False)

viewWidget (ViewDiv c v) = liftR2 (divClass c) $ viewWidget v

viewWidget (Views xs) = liftM mconcat $ mapM viewWidget xs

viewWidget _ = return mempty


zoneWidget :: (MonadWidget t m, Eq a)
  => Int -> a -> (Definition -> Maybe a) -> (a -> Definition)
  -> (Dynamic t a -> Editor t m (Variable t a)) -- note: probably should just be Event t a
  -> Editor t m (Event t [EnsembleRequest])
zoneWidget z a f g b = do
  ctx <- askContext
  let a' = fmap (IntMap.lookup z . zones . ensemble . ensembleC) ctx -- :: Dynamic t (Maybe Definition)
  let a'' = fmap (maybe Nothing f) a' -- :: Dynamic t (Maybe a)
  let a''' = fmap (maybe a id) a'' -- :: Dynamic t a
  a'''' <- liftR $ holdUniqDyn a''' -- not sure if this is really necessary, but probably does little harm?
  v <- b a''''
  return $ ((:[]) . WriteZone z . g) <$> localEdits v
