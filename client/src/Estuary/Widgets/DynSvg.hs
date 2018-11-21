{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.DynSvg (svgDisplay) where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Svg
import Data.Map

import Estuary.Types.SvgOp
import Estuary.RenderInfo

svgDisplay :: MonadWidget t m => Int -> Dynamic t RenderInfo -> m ()
svgDisplay z rInfo = do
  instructions <- mapDyn svgOps rInfo
  instructions' <- holdDyn [] $ fmapMaybe id $ updated $ nubDyn instructions
  let attrs = fromList [("class","svgDisplay"),("style","z-index:" ++ show z)]
  x <- mapDyn instructionsToWidgets instructions' -- Dynamic t (m ())
  svgAttr "svg" attrs $ dyn x
  return ()

instructionsToWidgets :: MonadWidget t m => [SvgOp] -> m ()
instructionsToWidgets = mapM_ instructionToWidget

instructionToWidget :: MonadWidget t m => SvgOp -> m ()

instructionToWidget (Line x1 y1 x2 y2 s) = svgAttr "line" attrs $ return ()
  where attrs = fromList [
         ("x1",show x1 ++ "%"),
         ("y1",show y1 ++ "%"),
         ("x2",show x2 ++ "%"),
         ("y2",show y2 ++ "%"),
         ("stroke-linecap",show (strokeLineCap s)),
         ("stroke-linejoin",show (strokeLineJoin s)),
         ("stroke-width",show (strokeWidth s) ++ "%"),
         ("stroke",show (strokeColor s))
         ]

instructionToWidget (Rect x y w h s) = svgAttr "rect" attrs $ return ()
  where attrs = fromList [
         ("x",show x ++ "%"),
         ("y",show y ++ "%"),
         ("width",show w ++ "%"),
         ("height",show h ++ "%"),
         ("stroke-linecap",show (strokeLineCap s)),
         ("stroke-linejoin",show (strokeLineJoin s)),
         ("stroke-width",show (strokeWidth s) ++ "%"),
         ("stroke",show (strokeColor s)),
         ("fill","green")
         ]
