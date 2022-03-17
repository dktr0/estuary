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


-- currentLoad:: MonadWidget t m => W t m (Event t Int)
-- currentLoad = do
--     r <- renderInfo
--     widgetBuildTime <- liftIO $ getCurrentTime
--     tick <- tickLossy 0.01 widgetBuildTime
--     currentLoad <- performEvent $ attachWith getLoad (current r) $ fmap _tickInfo_lastUTC tick
--     return currentLoad

-- getLoad:: MonadIO m => RenderInfo -> UTCTime -> m Int  
-- getLoad r _ = do 
--     let x = avgRenderLoad r 
--     return x


concentricCircleVisionWidget :: MonadWidget t m => W t m ()
concentricCircleVisionWidget =  mdo
    r <- renderInfo
    load <- holdUniqDyn $ fmap avgRenderLoad r
    dynText $ fmap (showt) load

    loadCircles load 

    return ()

loadCircles:: MonadWidget t m => Dynamic t Int -> m ()
loadCircles load = do

    let size = 25 :: Rational;
    let z = constDyn $ "z" =: "-8"
    let cx = constDyn $  "cx" =: ((showt size) <> "%") 
    let cy = constDyn $  "cy" =: ((showt size) <> "%") 

    let r1 = constDyn $ "r" =:  "13%" -- ((showt (floor (size*0.5))) <> "%") 
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
sizeCircle x = "r" =: (showt x)

colourCircle:: Int -> Map Text Text
colourCircle x 
    | x < 50 = "fill" =: "var(--primary-color)"
    | otherwise = "fill" =: "var(--transient-color)"