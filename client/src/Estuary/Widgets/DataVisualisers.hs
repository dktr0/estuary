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


currentLoad:: MonadWidget t m => W t m (Event t Int)
currentLoad = do
    r <- renderInfo
    widgetBuildTime <- liftIO $ getCurrentTime
    tick <- tickLossy 0.01 widgetBuildTime
    currentLoad <- performEvent $ attachWith getLoad (current r) $ fmap _tickInfo_lastUTC tick
    return currentLoad

getLoad:: MonadIO m => RenderInfo -> UTCTime -> m Int  
getLoad r _ = do 
    let x = avgRenderLoad r 
    return x


concentricCircleVisionWidget :: MonadWidget t m => W t m ()
concentricCircleVisionWidget =  mdo
    r <- renderInfo
    let load = fmap avgRenderLoad r
    -- dynText $ fmap (showt) load -- debugging text
    let svgA = constDyn svgAttrs

    elDynAttrNS (Just "http://www.w3.org/2000/svg") "svg" svgA $ do
        loadCircles load
    return ()

svgAttrs:: Map Text Text
svgAttrs = 
    let vB = "viewBox" =: "0 0 100 100"
        w = "width" =: "100%"
        h = "height" =: "100%"
        style = "style" =: "border: 5px solid var(--primary-color);"
    in mconcat [vB,w,h]


loadCircles:: MonadWidget t m => Dynamic t Int -> m ()
loadCircles load = do

    let coords = 50 :: Double;
    let z = constDyn $ "z" =: "-8"
    let cx = constDyn $  "cx" =: (showt $ coords) 
    let cy = constDyn $  "cy" =: (showt $ coords) 

    let r1 = constDyn $ "r" =:  (showt ((coords*0.5))) 
    let fill1 = constDyn $ "fill" =: "transparent"

    let r2 = sizeCircle <$> load
    let fill2 = colouring ("fill","var(--primary-color)") <$> load

    let (stroke1,stroke2,strokew) = (constDyn $ "stroke" =: "var(--secondary-color)",constDyn $ "stroke" =: "transparent",constDyn $ "stroke-width" =: "2")
    let externalCircle = mconcat [cx,cy,r1,fill1,stroke1,strokew,z]
    let innerCircle = mconcat [cx,cy,r2,fill2, stroke2, strokew,z]
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" externalCircle $ return ()
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" innerCircle $ return ()
    return ()

sizeCircle:: Int -> Map Text Text
sizeCircle x = "r" =: (showt (x'*0.5))
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
    elDynAttrNS (Just "http://www.w3.org/2000/svg") "svg" (constDyn svgAttrs) $ do
        loadGraph lista
    return ()

injectFact:: [Int] -> Int -> [Int]
injectFact xs x = Prelude.take 150 $ x:xs -- changing width means changing the amount of samples

loadGraph:: MonadWidget t m => Dynamic t [Int] -> m ()
loadGraph xs = do
    let widths = 150 :: Float 
    let z = constDyn $ "z" =: "-8"
    let fill = constDyn $  "fill" =: "none" 
    let stroke = constDyn $  "stroke" =: "var(--primary-color)"
    let strokeWidth = constDyn $ "stroke-width" =: "1"  
    let transf = constDyn $ "transform" =: ("rotate(180," <> (showt $ widths*0.5) <> ",50)")
    let pts = (points . Prelude.zip [0..]) <$> xs -- [(Int,Int)]
    let attrs = mconcat [z,fill,stroke,strokeWidth,pts,transf]

    let dynStroke = fmap ((colouring ("stroke","var(--secondary-color)")) . Prelude.head) xs
    let x1 = constDyn $ "x1" =: "0"
    let y1 = constDyn $ "y1" =: "50"
    let x2 = constDyn $ "x2" =: (showt widths)
    let y2 = constDyn $ "y2" =: "50"
    let attrs' = mconcat [z,dynStroke,x1,y1,x2,y2]

    let dynStroke = fmap ((colouring ("stroke","var(--secondary-color)")) . Prelude.head) xs
    let w = constDyn $ "width" =: (showt widths)
    let h = constDyn $ "height" =: "100"
    let attrs'' = mconcat [z,dynStroke,w,h]


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