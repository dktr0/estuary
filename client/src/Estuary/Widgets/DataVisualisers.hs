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
    load <- holdUniqDyn $ fmap avgRenderLoad r
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
    let fill2 = colourCircle <$> load

    let (stroke1,stroke2,strokew) = (constDyn $ "stroke" =: "var(--secondary-color)",constDyn $ "stroke" =: "transparent",constDyn $ "stroke-width" =: "2")
    let externalCircle = mconcat [cx,cy,r1,fill1,stroke1,strokew,z]
    let innerCircle = mconcat [cx,cy,r2,fill2, stroke2, strokew,z]
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" externalCircle $ return ()
    elDynAttrNS' (Just "http://www.w3.org/2000/svg") "circle" innerCircle $ return ()
    return ()

sizeCircle:: Int -> Map Text Text
sizeCircle x = "r" =: (showt (x'*0.5))
    where x' = (realToFrac x :: Double)

colourCircle:: Int -> Map Text Text
colourCircle x 
    | x < 50 = "fill" =: "var(--primary-color)"
    | otherwise = "fill" =: "var(--transient-color)"


graphVisionWidget :: MonadWidget t m => W t m ()
graphVisionWidget =  mdo
    r <- renderInfo
    let loadDyn = fmap avgRenderLoad r
    let loadEvent = updated $ loadDyn -- Event t Int
    lista <- accumDyn injectFact [] loadEvent  
    elDynAttrNS (Just "http://www.w3.org/2000/svg") "svg" (constDyn svgAttrs) $ do
        loadGraph lista
    return ()


-- accumDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> a) -> a -> Event t b -> m (Dynamic t a) 

-- accum :: (MonadHold t m, MonadFix m) => (a -> b -> a) -> a -> Event t b -> m (f a) 

injectFact:: [Int] -> Int -> [Int]
injectFact xs x = Prelude.take 100 $ x:xs

loadGraph:: MonadWidget t m => Dynamic t [Int] -> m ()
loadGraph xs = do
    let z = constDyn $ "z" =: "-8"
    let fill = constDyn $  "fill" =: "none" 
    let stroke = constDyn $  "stroke" =: "var(--primary-color)"
    let strokeWidth = constDyn $ "stroke-width" =: "1"  
    let transf = constDyn $ "transform" =: "rotate(180)"

    let pts = (points . Prelude.zip [0..]) <$> xs -- [(Int,Int)]

    let attrs = mconcat [z,fill,stroke,strokeWidth,pts,transf]
    elDynAttrNS (Just "http://www.w3.org/2000/svg") "polyline" attrs $ return ()
    return ()



-- <svg height="200" width="500">
--   <polyline fill="none" stroke="black" stroke-width="3" points="20,20 40,25 60,40 80,120 120,140 200,180" 
--   Sorry, your browser does not support inline SVG.
-- </svg>


    -------- points to make polygons or paths

points :: [(Int,Int)] -> Map Text Text
points [] = Data.Map.empty
points x = "points" =: (coordToText x)

coordToText:: [(Int,Int)] -> Text
coordToText p = Prelude.foldl (\ x y -> x <> " " <> (ptsToCoord y)) "" p

ptsToCoord:: (Int,Int) -> Text
ptsToCoord (x,y) = T.pack (show x) <> (T.pack ",") <> T.pack (show y)