{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.DynSvg where

import Reflex
import Reflex.Dom
import Data.Map
import Reflex.Dom.Contrib.Widgets.Svg
import Control.Monad
import GHCJS.DOM.EventM -- just used for our test, maybe delete-able later

data DrawingInstruction =
  Line Int Int Int Int |
  Rect Int Int Int Int

testOurDynSvg :: (MonadWidget t m) => m ()
testOurDynSvg = mdo
  xs <- mapDyn (\(x,y) -> [Line x y 0 0, Line x y 400 400] ) xy
  e <- dynSvg xs
  clickEv <- wrapDomEvent (_el_element e) (onEventName Click) mouseOffsetXY
  xy <- holdDyn (200,200) clickEv
  return ()

dynSvg :: (MonadWidget t m) => Dynamic t [DrawingInstruction] -> m (El t)
dynSvg x = do
  (e,_) <- el' "div" $ mapDyn (instructionsToWidget 400 400) x >>= dyn >> return ()
  return e

instructionsToWidget :: MonadWidget t m => Int -> Int -> [DrawingInstruction] -> m ()
instructionsToWidget w h xs = svgAttr "svg" attrs $ mapM instructionToWidget xs >> return ()
  where attrs = fromList [("width",show w),("height",show h)]

instructionToWidget :: MonadWidget t m => DrawingInstruction -> m ()

instructionToWidget (Line x1 y1 x2 y2) = svgAttr "line" attrs $ return ()
  where attrs = fromList [("x1",show x1),("y1",show y1),("x2",show x2),("y2",show y2),("stroke","white")]

instructionToWidget (Rect x y w h) = svgAttr "rect" attrs $ return ()
  where attrs = fromList [("x",show x),("y",show y),("width",show w),("height",show h),("stroke","white"),("fill","green")]
