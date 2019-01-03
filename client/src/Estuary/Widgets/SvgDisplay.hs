{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.SvgDisplay (svgDisplay) where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Svg
import Data.Map
import Data.Text as T

import Estuary.Types.Color
import Estuary.Types.Stroke
import Estuary.Types.Transform
import Estuary.Types.SvgOp
import Estuary.RenderInfo

svgDisplay :: MonadWidget t m => Int -> Dynamic t RenderInfo -> m ()
svgDisplay z rInfo = do
  instructions <- mapDyn svgOps rInfo
  instructions' <- holdDyn [] $ fmapMaybe id $ updated $ nubDyn instructions
  let attrs = fromList [("class","svgDisplay"),("style",T.pack $ "z-index:" ++ show z), ("viewBox", "0 0 100 100"), ("xmlns", "http://www.w3.org/2000/svg")]
  x <- mapDyn instructionsToWidgets instructions' -- Dynamic t (m ())
  svgAttr "svg" attrs $ dyn x
  return ()

--a helper function to get "x,y x,y"
listToString' :: [Double] -> String
listToString' [] = []
listToString' (x:y:zs) = Prelude.concat [show x, ",", show y, " ",  listToString' zs]

instructionsToWidgets :: MonadWidget t m => [SvgOp] -> m ()
instructionsToWidgets = mapM_ instructionToWidget

instructionToWidget :: MonadWidget t m => SvgOp -> m ()

instructionToWidget (Line x1 y1 x2 y2 s) = svgAttr "line" attrs $ return ()
  where attrs = fromList [
         ("x1",T.pack $ show x1 ++ "%"),
         ("y1",T.pack $ show y1 ++ "%"),
         ("x2",T.pack $ show x2 ++ "%"),
         ("y2",T.pack $ show y2 ++ "%"),
         ("stroke-linecap",T.pack $ show (strokeLineCap s)),
         ("stroke-linejoin",T.pack $ show (strokeLineJoin s)),
         ("stroke-width",T.pack $ show (strokeWidth s) ++ "%"),
         ("stroke",T.pack $ show (strokeColor s)),
         ("stroke-dasharray",T.pack $ show (strokeDashArray s))
         ]

instructionToWidget (Rect x y w h f s) = svgAttr "rect" attrs $ return ()
  where attrs = fromList [
         ("x",T.pack $ show x ++ "%"),
         ("y",T.pack $ show y ++ "%"),
         ("width",T.pack $ show w ++ "%"),
         ("height",T.pack $ show h ++ "%"),
         ("fill",T.pack $ show f),
         ("stroke-linecap",T.pack $ show (strokeLineCap s)),
         ("stroke-linejoin",T.pack $ show (strokeLineJoin s)),
         ("stroke-width",T.pack $ show (strokeWidth s) ++ "%"),
         ("stroke",T.pack $ show (strokeColor s)),
         ("stroke-dasharray",T.pack $ show (strokeDashArray s))
         ]

instructionToWidget (Circle x y r f s) = svgAttr "circle" attrs $ return ()
  where attrs = fromList [
         ("cx",T.pack $ show x ++ "%"),
         ("cy",T.pack $ show y ++ "%"),
         ("r",T.pack $ show r ++ "%"),
         ("fill",T.pack $  show f),
         ("stroke-linecap",T.pack $ show (strokeLineCap s)),
         ("stroke-linejoin",T.pack $ show (strokeLineJoin s)),
         ("stroke-width",T.pack $ show (strokeWidth s) ++ "%"),
         ("stroke",T.pack $ show (strokeColor s)),
         ("stroke-dasharray",T.pack $  show (strokeDashArray s))
         ]

instructionToWidget (Ellipse x y rx ry f s) = svgAttr "ellipse" attrs $ return ()
  where attrs = fromList [
         ("cx",T.pack $ show x ++ "%"),
         ("cy",T.pack $ show y ++ "%"),
         ("rx",T.pack $ show rx ++ "%"),
         ("ry",T.pack $ show ry ++ "%"),
         ("fill",T.pack $  show f),
         ("stroke-linecap",T.pack $ show (strokeLineCap s)),
         ("stroke-linejoin",T.pack $ show (strokeLineJoin s)),
         ("stroke-width",T.pack $ show (strokeWidth s) ++ "%"),
         ("stroke",T.pack $ show (strokeColor s)),
         ("stroke-dasharray",T.pack $  show (strokeDashArray s))
         ]

instructionToWidget (Triangle ax ay bx by cx cy f s) = svgAttr "polygon" attrs $ return ()
  where attrs = fromList [
         ("points",T.pack $  show ax ++ "," ++ show ay ++ " " ++ show bx ++ "," ++ show by ++ " " ++ show cx ++ "," ++ show cy),
         ("fill", T.pack $ show f),
         ("stroke-linecap",T.pack $ show (strokeLineCap s)),
         ("stroke-linejoin",T.pack $ show (strokeLineJoin s)),
         ("stroke-width",T.pack $ show (strokeWidth s) ++ "%"),
         ("stroke",T.pack $ show (strokeColor s)),
         ("stroke-dasharray", T.pack $ show (strokeDashArray s))
         ]

instructionToWidget (Polyline p f s) = svgAttr "polyline" attrs $ return ()
  where attrs = fromList [
         ("points",T.pack $ listToString' p),
         ("fill", T.pack $ show f),
         ("stroke-linecap",T.pack $ show (strokeLineCap s)),
         ("stroke-linejoin",T.pack $ show (strokeLineJoin s)),
         ("stroke-width",T.pack $ show (strokeWidth s) ++ "%"),
         ("stroke",T.pack $ show (strokeColor s)),
         ("stroke-dasharray",T.pack $  show (strokeDashArray s))
         ]

instructionToWidget (Polygon p f s) = svgAttr "polygon" attrs $ return ()
  where attrs = fromList [
         ("points",T.pack $ listToString' p),
         ("fill", T.pack $ show f),
         ("stroke-linecap",T.pack $ show (strokeLineCap s)),
         ("stroke-linejoin",T.pack $ show (strokeLineJoin s)),
         ("stroke-width",T.pack $ show (strokeWidth s) ++ "%"),
         ("stroke",T.pack $ show (strokeColor s)),
         ("stroke-dasharray",T.pack $  show (strokeDashArray s))
            ]
