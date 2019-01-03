{-# LANGUAGE RecursiveDo, GADTs, DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Estuary.Tutorials.Tutorial where

import Control.Monad (liftM)

import qualified Data.IntMap.Strict as IM
import Data.Map as M
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T

import Estuary.RenderInfo
import Estuary.Types.Context
import Estuary.Types.Definition
import Estuary.Types.Hint
import Estuary.Types.Language
import Estuary.Types.Request
import Estuary.Types.View
import Estuary.Widgets.Generic
import Estuary.Widgets.View
import Estuary.Widgets.Text (textNotationWidget)
import Estuary.Types.TidalParser
import Estuary.Types.TextNotation
import GHC.Generics

import GHCJS.Marshal

import Reflex
import Reflex.Dom

import Estuary.Types.Language
import Estuary.Tidal.Types
import Estuary.Types.Live
import Estuary.Widgets.Generic (clickableDivAttrs)
import Estuary.Reflex.Utility (buttonDynAttrs)

-- ! Don't forget to add show instance when adding new tutorial
data TutorialId = IntroTidalText deriving (Eq, Ord, Generic, FromJSVal, ToJSVal)

instance Show TutorialId where
  show IntroTidalText = "A Brief Introduction to TidalCycles (MiniTidal)"
  show _ = "<tutorial>"

data Tutorial t m = Tutorial {
  tutorialId::TutorialId,
  widget::(Dynamic t Context -> m (Dynamic t DefinitionMap, Event t Hint))
}


attachIndex:: [a] -> [(Int,a)]
attachIndex l = zip (take (length l) [0..]) l

dynList::MonadWidget t m => [Dynamic t a] -> m (Dynamic t [a])
dynList l = mapDyn elems $ joinDynThroughMap $ constDyn $ fromList $ attachIndex l

title::MonadWidget t m => m a -> m a
title widget = elClass "div" "title" widget

labelWidget::MonadWidget t m => Dynamic t Context -> M.Map Language Text -> m ()
labelWidget ctx txt = do
  let dflt = safeHead "" $ elems txt
  str <- mapDyn (\c-> maybe dflt id $ M.lookup (language c) txt) ctx
  dynText str
  where
    safeHead a [] = a
    safeHead _ (x:xs) = x

miniTidalWidget :: MonadWidget t m => Dynamic t Context -> Int -> Int -> String -> m (Dynamic t (Int, Definition), Event t Hint)
miniTidalWidget ctx rows index initialText = elClass "div" "panel" $ do
  let silent = (Live (TidalTextNotation MiniTidal,"") L3)
  pb <- liftM (silent <$) $ getPostBuild -- TODO PB used to block things from playing immediately.. should probably be less hacky about this
  shh <- buttonDynAttrs "silence" silent (constDyn $ M.fromList [("style","float:right")])
  (v, _, hints) <- textNotationWidget ctx (constDyn Nothing) rows (Live (TidalTextNotation MiniTidal, initialText) L3) never
  v' <- holdDyn (Live (TidalTextNotation MiniTidal, initialText) L3) $  leftmost [pb, updated v, shh]
  defn <- mapDyn (\v-> (index,(TextProgram v))) v'
  return (defn, hints)
