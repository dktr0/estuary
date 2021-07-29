{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
module Estuary.Widgets.SpecificPattern where

import Reflex
import Reflex.Dom hiding (Delete,Insert,End)
import Data.Map
import Control.Monad
import Data.List(intersperse, findIndex)
import GHCJS.DOM.EventM
import Data.Maybe(isJust)
import Text.Read(readMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.Types.Hint
import Estuary.Types.Live
import Estuary.Widgets.Reflex
import qualified Estuary.Widgets.GeneralPattern as G
import Estuary.Tidal.Types


specificContainer :: MonadWidget t m => SpecificPattern -> Event t () -> m (Dynamic t (SpecificPattern, Event t (), Event t Hint))
specificContainer (Accelerate x) e = G.generalContainerLive' (G.popupDoubleWidget 0 (-500) 500 1) x never >>= (return . fmap (\(x,ev,h)->(Accelerate x,(() <$) ev,h)))
specificContainer (Bandq x) e = G.generalContainerLive'  (G.popupDoubleWidget 1 0 22000 10) x never >>= (return . fmap (\(x,ev,h)->(Bandq x,(() <$) ev,h)))
specificContainer (Begin x) e = G.generalContainerLive'  (G.popupDoubleWidget 0 0 1 0.05) x never >>= (return . fmap (\(x,ev,h)->(Begin x,(() <$) ev,h)))
specificContainer (Delay x) e = G.generalContainerLive'    (G.popupDoubleWidget 0.5 0 1 0.05) x never >>= (return . fmap (\(x,ev,h)->(Delay x,(() <$) ev,h)))
specificContainer (Delayfeedback x) e = G.generalContainerLive' (G.popupDoubleWidget 0.2 0 1 0.05) x never >>= (return . fmap (\(x,ev,h)->(Delayfeedback x,(() <$) ev,h)))
specificContainer (Delaytime x) e = G.generalContainerLive' (G.popupDoubleWidget 0.5 0 1 0.05) x never >>= (return . fmap (\(x,ev,h)->(Delaytime x,(() <$) ev,h)))
specificContainer (End x) e = G.generalContainerLive' (G.popupDoubleWidget 1 0 1 0.05) x never >>= (return . fmap (\(x,ev,h)->(End x,(() <$) ev,h)))
specificContainer (Gain x) e = G.generalContainerLive' (G.popupDoubleWidget 1 0 1 0.05) x never >>= (return . fmap (\(x,ev,h)->(Gain x,(() <$) ev,h)))
specificContainer (Hresonance x) e = G.generalContainerLive' (G.popupDoubleWidget 0 0 1 0.05) x never >>= (return . fmap (\(x,ev,h)->(Hresonance x,(() <$) ev,h)))
specificContainer (Pan x) e = G.generalContainerLive' (G.popupDoubleWidget 0.5 0 1 0.05) x never >>= (return . fmap (\(x,ev,h)->(Pan x,(() <$) ev,h)))
specificContainer (Resonance x) e = G.generalContainerLive' (G.popupDoubleWidget 0 0 1 0.05) x never >>= (return . fmap (\(x,ev,h)->(Resonance x,(() <$) ev,h)))

specificContainer (Shape x) e = do
  y <- G.generalContainerLive' (G.popupDoubleWidget 0 0 1 0.05) x never
  return $ fmap (\(x,ev,h)->(Shape x,(() <$) ev,h)) y

specificContainer (Speed x) e = do
  y <- G.generalContainerLive' (G.typedAtomWidget 1) x never
  return $ fmap (\(x,ev,hint)->(Speed x,(() <$) ev,hint)) y

specificContainer (Up x) e = do
  y <- G.generalContainerLive' (G.popupDoubleWidget 0 (-132) 132 1) x never
  return $ fmap (\(x,ev,h)->(Up x,(() <$) ev,h)) y

specificContainer (Bandf x) e = G.generalContainerLive' (G.popupIntWidget 440 0 25000 10) x never >>= (return . fmap (\(x,ev,h)->(Bandf x,(() <$) ev,h)))
specificContainer (Coarse x) e = G.generalContainerLive' (G.popupIntWidget 0 0 24 1) x never >>= (return . fmap (\(x,ev,h)->(Coarse x,(() <$) ev,h)))
specificContainer (Crush x) e = G.generalContainerLive' (G.popupIntWidget 16 0 16 1) x never >>= (return . fmap (\(x,ev,h)->(Crush x,(() <$) ev,h)))
specificContainer (Estuary.Tidal.Types.Cut x) e = G.generalContainerLive' (G.popupIntWidget 1 (-50) 50 1) x never >>= (return . fmap (\(x,ev,h)->(Estuary.Tidal.Types.Cut x,(() <$) ev,h)))
specificContainer (Cutoff x) e = G.generalContainerLive' (G.popupIntWidget 20000 0 25000 10) x never >>= (return . fmap (\(x,ev,h)->(Cutoff x,(() <$) ev,h)))
specificContainer (Hcutoff x) e = G.generalContainerLive' (G.popupIntWidget 0 0 25000 10) x never >>= (return . fmap (\(x,ev,h)->(Hcutoff x,(() <$) ev,h)))
specificContainer (Loop x) e = G.generalContainerLive' (G.popupIntWidget 0 0 1024 1) x never >>= (return . fmap (\(x,ev,h)->(Loop x,(() <$) ev,h)))
specificContainer (N x) e = G.generalContainerLive' (G.popupIntWidget 0 0 50 1) x never >>= (return . fmap (\(x,ev,h)->(N x,(() <$) ev,h)))

specificContainer (S x) e = do
  G.generalContainerLive' (G.typedAtomWidget "~") x never >>= (return . fmap (\(x,ev,hint)->(S x,(() <$) ev,hint)))
specificContainer (Vowel x) e = G.generalContainerLive' G.charWidget x never >>= (return . fmap (\(x,ev,h)->(Vowel x,(() <$) ev,h)))
specificContainer (Unit x) e = G.generalContainerLive' G.charWidget x never >>= (return . fmap (\(x,ev,h)->(Unit x,(() <$) ev,h)))
