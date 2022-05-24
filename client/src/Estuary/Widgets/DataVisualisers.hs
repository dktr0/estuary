{-# LANGUAGE RecursiveDo, OverloadedStrings #-}

module Estuary.Widgets.DataVisualisers where

import Reflex
import Reflex.Dom
import Control.Monad.Trans
import Text.Read
import Data.Text
import Data.Time
import Data.Map

import Sound.MusicW.AudioContext
import Data.Text (Text)
import qualified Data.Text as T
import TextShow
-- import qualified Sound.Tidal.Bjorklund as TBj

import Estuary.Types.Tempo
import Estuary.Types.Context
import Estuary.Types.EnsembleResponse
import Estuary.Widgets.Text
import Estuary.Widgets.Reflex
import qualified Estuary.Types.Term as Term
import Estuary.Types.Language
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Widgets.W
import Estuary.Types.Definition
import Estuary.Types.RenderInfo 

------ svg attributes for different visualisers
svgAttrsGraph:: Map Text Text
svgAttrsGraph = 
    let vB = "viewBox" =: "0 0 1000 600"
        par = "preserveAspectRatio" =: "none" 
        w = "width" =: "100%"
        h = "height" =: "100%"
    in mconcat [vB,w,h,par]

svgAttrsCircle:: Map Text Text
svgAttrsCircle = 
    let vB = "viewBox" =: "0 0 1000 600"
        par = "preserveAspectRatio" =: "xMidYMid meet" 
        w = "width" =: "100%"
        h = "height" =: "100%"
    in mconcat [vB,w,h,par]

svgAttrsVintage:: Map Text Text
svgAttrsVintage = 
    let vB = "viewBox" =: "0 0 1000 600"
        par = "preserveAspectRatio" =: "XMidyMid meet" 
        w = "width" =: "100%"
        h = "height" =: "100%"
    in mconcat [vB,w,h,par]


concentricCircleVisionWidget :: MonadWidget t m => W t m ()
concentricCircleVisionWidget =  mdo
    r <- renderInfo
    let load = fmap avgRenderLoad r
    -- dynText $ fmap (showt) load -- debugging text
    let svgA = constDyn svgAttrsCircle

    elDynAttrNS (Just "http://www.w3.org/2000/svg") "svg" svgA $ do  -- broken, adapt to new viewBox
        loadCircles load
    return ()

loadCircles:: MonadWidget t m => Dynamic t Int -> m ()
loadCircles load = do

    let x = constDyn $ "x" =: "1"
    let y = constDyn $ "y" =: "1"
    let w = constDyn $ "width" =: "998"
    let h = constDyn $ "height" =: "599"
    let st = constDyn $ "stroke" =: "var(--secondary-color)"
    let f = constDyn $ "fill" =: "transparent"
    let rectAttrs = mconcat [x,y,w,h,st,f]

    let coords = 100 :: Double;
    let z = constDyn $ "z" =: "0"
    let cx = constDyn $  "cx" =: (showt $ (coords*5)) 
    let cy = constDyn $  "cy" =: (showt $ (coords*3)) 
    let r1 = constDyn $ "r" =:  (showt ((coords*3*0.5))) 
    let fill1 = constDyn $ "fill" =: "transparent"
    let r2 = sizeCircle <$> load
    let fill2 = colouring ("fill","var(--primary-color)") <$> load
    let (stroke1,stroke2,strokew) = (constDyn $ "stroke" =: "var(--secondary-color)",constDyn $ "stroke" =: "transparent",constDyn $ "stroke-width" =: "2")
    let externalCircle = mconcat [cx,cy,r1,fill1,stroke1,strokew,z]
    let innerCircle = mconcat [cx,cy,r2,fill2, stroke2, strokew,z]
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "rect" rectAttrs $ return ()
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" externalCircle $ return ()
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" innerCircle $ return ()
    return ()

sizeCircle:: Int -> Map Text Text
sizeCircle x 
    | x > 100 = "r" =: "300"
    | otherwise = "r" =: (showt (x'*3))
                where x' = (realToFrac x :: Double)

colouring:: (Text,Text) -> Int -> Map Text Text
colouring (t,c) x 
    | x < 50 = t =: c
    | otherwise = t =: "var(--transient-color)"


graphVisionWidget :: MonadWidget t m => W t m ()
graphVisionWidget =  mdo
    r <- renderInfo
    let loadDyn = fmap avgRenderLoad r
    let loadEvent = updated $ loadDyn -- Event t Int
    lista <- accumDyn injectFact [0] loadEvent  
    elDynAttrNS (Just "http://www.w3.org/2000/svg") "svg" (constDyn svgAttrsGraph) $ do  -- broken adapt to new viewBox
        loadGraph lista
    return ()

injectFact:: [Int] -> Int -> [Int]
injectFact xs x = Prelude.take 500 $ x:xs -- changing width means changing the amount of samples

scaleIntegers:: Double -> [Int] -> [Int] 
scaleIntegers factor xs = Prelude.map (\x -> round $ ((realToFrac x :: Double) * factor) :: Int ) xs


loadGraph:: MonadWidget t m => Dynamic t [Int] -> m ()
loadGraph xs = do
    let widths = 1000 :: Float 
    let z = constDyn $ "z" =: "0"
    let fill = constDyn $  "fill" =: "none" 
    let stroke = constDyn $  "stroke" =: "var(--primary-color)"
    let strokeWidth = constDyn $ "stroke-width" =: "1"  
    let transf = constDyn $ "transform" =: ("rotate(180," <> (showt $ widths*0.5) <> ",300)")
    let pts = (points . Prelude.zip [0,2..]) <$> fmap (scaleIntegers 6) xs -- [(Int,Int)]
    let attrs = mconcat [z,fill,stroke,strokeWidth,pts,transf]

    let dynStroke = fmap ((colouring ("stroke","var(--secondary-color)")) . Prelude.head) xs
    let x1 = constDyn $ "x1" =: "0"
    let y1 = constDyn $ "y1" =: "300"
    let x2 = constDyn $ "x2" =: (showt widths)
    let y2 = constDyn $ "y2" =: "300"
    let attrs' = mconcat [z,dynStroke,x1,y1,x2,y2]

    let stroke' = constDyn $  "stroke" =: "var(--secondary-color)"
    let w = constDyn $ "width" =: (showt widths)
    let h = constDyn $ "height" =: "600"
    let attrs'' = mconcat [z,stroke',w,h]


    elDynAttrNS (Just "http://www.w3.org/2000/svg") "rect" attrs'' $ return ()
    elDynAttrNS (Just "http://www.w3.org/2000/svg") "polyline" attrs $ return ()
    elDynAttrNS (Just "http://www.w3.org/2000/svg") "line" attrs' $ return ()
    return ()



    -------- points to make polygons or paths

points :: [(Int,Int)] -> Map Text Text
points [] = Data.Map.empty
points x = "points" =: (coordToText x)

coordToText:: [(Int,Int)] -> Text
coordToText p = Prelude.foldl (\ x y -> x <> " " <> (ptsToCoord y)) "" p

ptsToCoord:: (Int,Int) -> Text
ptsToCoord (x,y) = T.pack (show x) <> (T.pack ",") <> T.pack (show y)


vintageVisionWidget:: MonadWidget t m => W t m ()
vintageVisionWidget = mdo
    r <- renderInfo
    let load = fmap avgRenderLoad r
    -- dynText $ fmap (showt) load -- debugging text
    let svgA = constDyn svgAttrsVintage

    elDynAttrNS (Just "http://www.w3.org/2000/svg") "svg" svgA $ do
        loadCockpit load
    return ()


loadCockpit:: MonadWidget t m => Dynamic t Int -> m ()
loadCockpit load = do
    let x1 = constDyn $ "x1" =: "0"
    let y1 = constDyn $ "y1" =: "700"
    let x2 = constDyn $ "x2" =: "500"
    let y2 = constDyn $ "y2" =: "700"
    let stroke = constDyn $ "stroke" =: "var(--primary-color)"
    let strokeW = constDyn $ "stroke-width" =: "10"
    let opacity = constDyn $ "opacity" =: "0.75"
    let transform = needle <$> load
    let lAttrs = mconcat [x1,y1,x2,y2,stroke,strokeW,opacity,transform]
    path
    simpleList (constDyn (marksForVisualiser ["10","20","30","40","50","60","70","80","90","100"])) tx
    elDynAttrNS (Just "http://www.w3.org/2000/svg") "line" lAttrs $ return ()
    return ()

needle:: Int -> Map Text Text
needle load 
    | load > 100 = "transform" =: ("rotate(146.6667,500,700)")
    | otherwise =  "transform" =: ("rotate(" <> (showt l) <> ",500,700)")
                    where l = round (33 + ((realToFrac load :: Rational)*0.01*113.6667)) :: Int

marksForVisualiser:: [Text] -> [(Text,Int)]
marksForVisualiser x = h : (Prelude.zip x $ (\seg -> iterate (+seg) (seg+alignNeedleOffset)) (round (vBWidth/(realToFrac n :: Rational)) :: Int))
    where n = Prelude.length x
          vBWidth = 1000
          h = ("0",alignNeedleOffset)
          alignNeedleOffset = 38

path:: MonadWidget t m => m ()
path = do
    let id = constDyn $ "id" =: "percen"
    let stroke = constDyn $ "stroke" =: "var(--secondary-color)"
    let strokeW = constDyn $ "stroke-width" =: "350"
    let d = constDyn $ "d" =: "m0,400 c200,-300 800,-300 1000,0"
    let attrs = mconcat [id,stroke,strokeW,d]
    elDynAttrNS (Just "http://www.w3.org/2000/svg") "path" attrs $ return ()
    return ()


tx:: MonadWidget t m => Dynamic t (Text,Int) -> m ()
tx x = do
--    let txLength = constDyn $ "textLength" =: "1030" might be useful at some point
    let texto = fmap fst x
    let offset = fmap snd x

    let fontS = constDyn $ "font-size" =: "3em"
    let fill = constDyn $ "fill" =: "var(--background-color)"
    let startOffset = generateAttr "startOffset" <$> offset  -- make dyn t map text text /// 
    let href = constDyn $ "href" =: "#percen"
    let txAttrs = mconcat [fontS, fill]
    let txPAttrs = mconcat [startOffset, href]
    elDynAttrNS (Just "http://www.w3.org/2000/svg") "text" txAttrs $ do
        elDynAttrNS (Just "http://www.w3.org/2000/svg") "textPath" txPAttrs $ tex texto 
    return ()

tex:: MonadWidget t m => Dynamic t Text -> m ()
tex x = do 
    t <- sample $ current x
    text t
    return ()

generateAttr :: Text -> Int -> Map Text Text
generateAttr atr x = atr =: (showt (realToFrac x :: Double))