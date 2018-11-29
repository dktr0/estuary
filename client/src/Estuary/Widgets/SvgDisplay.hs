{-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.SvgDisplay (svgDisplay) where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Svg
import Data.Map

import Estuary.Types.Color
import Estuary.Types.Stroke
import Estuary.Types.Transform
import Estuary.Types.SvgOp
import Estuary.RenderInfo

svgDisplay :: MonadWidget t m => Int -> Dynamic t RenderInfo -> m ()
svgDisplay z rInfo = do
  instructions <- mapDyn svgOps rInfo
  instructions' <- holdDyn [] $ fmapMaybe id $ updated $ nubDyn instructions
  let attrs = fromList [("class","svgDisplay"),("style","z-index:" ++ show z), ("viewBox", "0 0 100 100"), ("xmlns", "http://www.w3.org/2000/svg")]
  x <- mapDyn instructionsToWidgets instructions' -- Dynamic t (m ())
  svgAttr "svg" attrs $ dyn x
  return ()

--a helper function to get "x,y x,y"
listToString' :: [Double] -> String
listToString' [] = []
listToString' (x:y:zs) = concat [show x, ",", show y, " ",  listToString' zs]

instructionsToWidgets :: MonadWidget t m => [SvgOp] -> m ()
instructionsToWidgets = mapM_ instructionToWidget

instructionToWidget :: MonadWidget t m => SvgOp -> m ()

instructionToWidget (Line x1 y1 x2 y2 s t) = svgAttr "line" attrs $ return ()
  where attrs = fromList [
         ("x1",show x1 ++ "%"),
         ("y1",show y1 ++ "%"),
         ("x2",show x2 ++ "%"),
         ("y2",show y2 ++ "%"),
         ("stroke-linecap",show (strokeLineCap s)),
         ("stroke-linejoin",show (strokeLineJoin s)),
         ("stroke-width",show (strokeWidth s) ++ "%"),
         ("stroke",show (strokeColor s)),
         ("stroke-dasharray", show (strokeDashArray s)),
         ("style", "transform:" ++ show (tRotate t) ++ show (tScale t) ++ show (tSkew t) ++ show (tTranslate t))
         ]

instructionToWidget (Rect x y w h f s t) = svgAttr "rect" attrs $ return ()
  where attrs = fromList [
         ("x",show x ++ "%"),
         ("y",show y ++ "%"),
         ("width",show w ++ "%"),
         ("height",show h ++ "%"),
         ("fill", show f),
         ("stroke-linecap",show (strokeLineCap s)),
         ("stroke-linejoin",show (strokeLineJoin s)),
         ("stroke-width",show (strokeWidth s) ++ "%"),
         ("stroke",show (strokeColor s)),
         ("stroke-dasharray", show (strokeDashArray s)),
         ("style", "transform:" ++ show (tRotate t) ++ show (tScale t) ++ show (tSkew t) ++ show (tTranslate t))
         ]

instructionToWidget (Circle x y r f s t) = svgAttr "circle" attrs $ return ()
  where attrs = fromList [
         ("cx",show x ++ "%"),
         ("cy",show y ++ "%"),
         ("r",show r ++ "%"),
         ("fill", show f),
         ("stroke-linecap",show (strokeLineCap s)),
         ("stroke-linejoin",show (strokeLineJoin s)),
         ("stroke-width",show (strokeWidth s) ++ "%"),
         ("stroke",show (strokeColor s)),
         ("stroke-dasharray", show (strokeDashArray s)),
         ("style", "transform:" ++ show (tRotate t) ++ show (tScale t) ++ show (tSkew t) ++ show (tTranslate t))
         ]

instructionToWidget (Ellipse x y rx ry f s t) = svgAttr "ellipse" attrs $ return ()
  where attrs = fromList [
         ("cx",show x ++ "%"),
         ("cy",show y ++ "%"),
         ("rx",show rx ++ "%"),
         ("ry",show ry ++ "%"),
         ("fill", show f),
         ("stroke-linecap",show (strokeLineCap s)),
         ("stroke-linejoin",show (strokeLineJoin s)),
         ("stroke-width",show (strokeWidth s) ++ "%"),
         ("stroke",show (strokeColor s)),
         ("stroke-dasharray", show (strokeDashArray s)),
         ("style", "transform:" ++ show (tRotate t) ++ show (tScale t) ++ show (tSkew t) ++ show (tTranslate t))

         ]

instructionToWidget (Triangle ax ay bx by cx cy f s t) = svgAttr "polygon" attrs $ return ()
  where attrs = fromList [
         ("points", show ax ++ "," ++ show ay ++ " " ++ show bx ++ "," ++ show by ++ " " ++ show cx ++ "," ++ show cy),
         ("fill", show f),
         ("stroke-linecap",show (strokeLineCap s)),
         ("stroke-linejoin",show (strokeLineJoin s)),
         ("stroke-width",show (strokeWidth s) ++ "%"),
         ("stroke",show (strokeColor s)),
         ("stroke-dasharray", show (strokeDashArray s)),
         ("style", "transform:" ++ show (tRotate t) ++ show (tScale t) ++ show (tSkew t) ++ show (tTranslate t))
         ]

instructionToWidget (Polyline p f s t) = svgAttr "polyline" attrs $ return ()
  where attrs = fromList [
         ("points", (listToString' p)),
         ("fill", show f),
         ("stroke-linecap",show (strokeLineCap s)),
         ("stroke-linejoin",show (strokeLineJoin s)),
         ("stroke-width",show (strokeWidth s) ++ "%"),
         ("stroke",show (strokeColor s)),
         ("stroke-dasharray", show (strokeDashArray s)),
         ("style", "transform:" ++ show (tRotate t) ++ show (tScale t) ++ show (tSkew t) ++ show (tTranslate t))
                ]

instructionToWidget (Polygon p f s t) = svgAttr "polygon" attrs $ return ()
  where attrs = fromList [
         ("points", (listToString' p)),
         ("fill", show f),
         ("stroke-linecap",show (strokeLineCap s)),
         ("stroke-linejoin",show (strokeLineJoin s)),
         ("stroke-width",show (strokeWidth s) ++ "%"),
         ("stroke",show (strokeColor s)),
         ("stroke-dasharray", show (strokeDashArray s)),
         ("style", "transform:" ++ show (tRotate t) ++ show (tScale t) ++ show (tSkew t) ++ show (tTranslate t))
                ]
