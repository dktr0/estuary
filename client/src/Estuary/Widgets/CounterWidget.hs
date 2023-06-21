-- {-# LANGUAGE OverloadedStrings, RecursiveDo #-}

-- module Estuary.Widgets.CounterWidget where

-- import Reflex
-- import Reflex.Dom
-- import Data.Time
-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import Text.Read
-- import TextShow
-- import Control.Monad
-- import Control.Monad.IO.Class
-- import Data.Map

-- import Estuary.Widgets.Reflex
-- import Estuary.Widgets.W
-- import Estuary.Types.Definition
-- import Estuary.Widgets.Text


-- type Display = Int

-- data CountDown =
--   Holding (Either Int Rational) Bool Display |  -- target loop and type of time
--   Falling (Either Int UTCTime) Bool Display  -- target and start time
--   deriving (Eq,Show,Generic)


-- --sandClockWidget :: MonadWidget t m => Dynamic t TimerDownState -> W t m (Variable t TimerDownState)
-- ------ Interface widget ---
-- uiWidget:: MonadWidget t m => Dynamic t CountDown -> W t m (Variable t CountDown)
-- uiWidget delta = divClass "interactiveArea" $  mdo 

--     let initialText = "initial count is 60, change it here"
--     let updatedText = constDyn "2666"
--     let editable = True
--     textos <- holdDyn initialText $ updatedText

--     (valTxBx,eventTxBx) <- (textToInvisible 1 editable textos) >>= divClass "txAreaDiv" -- esto es:: (Dynamic t Text, Event t Text)

--     loopMode <- (middlePannel x y z)>>= divClass "mid-centre-InteractiveArea" -- esto debe ser:: Event t (Bool,Bool)


--     (timer) <- bottomPanel myTimer >>= divClass "timeDisplay" -- esto debe ser:: Event t Timer (Aqui se cambia de Holding a Falling y viceversa)

--     v <- variable delta never
--     return v


-- middlePanel:: MonadWidget t m => W t m (Event t (Bool,Bool))  


-- break :: (Char -> Bool) -> Text -> (Text, Text) 

-- -- parseMinutes:: Text -> Text -- 13:00
-- -- parseMinutes x = 
-- --   let (m,s) = break (==':') x -- ("13","":00")
-- --       toSeconds = $ if (T.tail m == []) then [0] else (T.tail m)
  

-- changeLoop:: CountDown -> CountDown -- this is for the loop/unloop svg div
-- changeLoop (Holding x True y) = Holding x False y
-- changeLoop (Falling x False y) = Falling x True y
-- changeLoop (Holding x False y) = Holding x True y
-- changeLoop (Falling x True y) = Falling x False y

-- toCycles:: CountDown -> CountDown  -- this is for the clickableDiv of the cycle measure unit
-- toCycles (Holding (Left x) y z) = Holding (Right x) y z
-- toCycles (Falling (Left x) y z) = Falling (Right x) y z

-- toSeconds:: CountDown -> CountDown  -- this is for the clickableDiv of the time measure unit
-- toSeconds (Holding (Right x) y z) = Holding (Left x) y z
-- toSeconds (Falling (Right x) y z) = Falling (Left x) y z




-- -- mergeWith :: Reflex t => (a -> a -> a) -> [Event t a] -> Event t a
-- -- Create a new Event that occurs if at least one of the Events in the list occurs. If multiple occur at the same time they are folded from the left with the given function.

-- bottomPanel:: MonadWidget t m => CountDown -> W t m CountDown 
-- bottomPanel t = divClass "notYetSure" $ do
--     -- here goes a clickableDiv

--     newTimer <- Holding 0 False False -- clickableDiv returns a Timer (this Timer should be erased once the clickableDiv pathway is open)


--     beatPosition <- currentBeat

    

--     visualiseCountDownWidget cuenta

--     return newTimer 



-- -- textArea for timer
-- -- choose measures/cycles or time
-- -- display the countdown in text
-- -- loop or not   ------------------- modify definition to include loop and time-modality ??
-- <div class="timeDisplay">
--     <svg viewBox="0 0 100 100" width="100%" height="auto">
--         <text x="0" y="60" fill="black" font-size="1em"> 13:00 </text>
--     </svg>
-- </div>



-- visualiseCountDownWidget :: MonadWidget t m => Dynamic t Int -> W t m ()
-- visualiseCountDownWidget cuenta = do
--   let vB = constDyn $ "viewBox" =: "0 0 100 100"
--   let w = constDyn $ "width" =: "100%"
--   let h = constDyn $ "height" =: "auto"
--   let par = constDyn $ "preserveAspectRatio" =: "none" 
--   let attrs = mconcat [vB,w,h]
--   elDynAttrNS (Just "http://www.w3.org/2000/svg") "svg" attrs $ do
--         svgText $ fmap (countDownToDisplay) cuenta
--       return ()
--   return ()



-- playIcon = blank

-- svgText:: MonadWidget t m => Dynamic (Maybe Text) -> m ()
-- svgText Nothing = return playIcon
-- svgText (Just tx) = do
--     let x = "x" =: "0"
--     let y = "y" =: "60%"
--     let stroke = "stroke" =: "var(--primary-color)"
--     let fs = "font-size" =: "1em"
--     elDynAttrNS (Just "http://www.w3.org/2000/svg") "svg" attrs $ fmap (text) tx $ return () 


-- countDownToDisplay:: CountDown -> UTCTime -> Maybe Text
-- countDownToDisplay (Holding _ _ _) _ = Nothing
-- countDownToDisplay (Falling cuenta start loop mode) now = if xx < 0 then Just $ diffTimeToText 0 else Just $ diffTimeToText xx 
--                                  where xx = (diffUTCTime (addUTCTime (realToFrac cuenta) start) now)


-- -- general helpers

-- diffTimeToText :: NominalDiffTime -> Text
-- diffTimeToText x = showt (floor x `div` 60 :: Int) <> ":" <> (add0Mod x)

-- add0Mod:: NominalDiffTime -> Text
-- add0Mod x = if modulo < 10 then ("0" <> (showt modulo)) else showt modulo 
--   where modulo = (floor x) `mod` (60 :: Int)


-- getElapsedBeats :: MonadIO m => Tempo -> UTCTime -> m Rational
-- getElapsedBeats t now = do
--   let x = timeToCount t now 
--   return x  

-- currentBeat:: MonadWidget t m => W t m (Event t Rational)
-- currentBeat = do
--   c <- context
--   let currentTempo = fmap (tempo . ensemble . ensembleC) c
--   widgetBuildTime <- liftIO $ getCurrentTime
--   tick <- tickLossy 0.01 widgetBuildTime
--   beatPosition <- performEvent $ attachWith getElapsedBeats (current currentTempo) $ fmap _tickInfo_lastUTC tick
--   return beatPosition