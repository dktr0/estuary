{-# LANGUAGE OverloadedStrings, RecursiveDo #-}

module Estuary.Widgets.TunningEstErv where

import Reflex
import Reflex.Dom
import Data.Time
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read
import TextShow
import Control.Monad
import Control.Monad.IO.Class
import Data.Map

import Estuary.Widgets.Reflex
import Estuary.Widgets.W
import Estuary.Types.Definition
import Estuary.Widgets.Text


                              --numNotas --period
-- data Tunning = EdxTunning Rational Rational deriving (Show, Eq)  -- agregar CPSScale cuando este lista

edxScaleWidget:: MonadWidget t m => Dynamic t Tuning -> W t m (Variable t Tuning)
edxScaleWidget delta = divClass "tunning" $ mdo

    let initialEDText = "equal division number goes here"
    let updatedEDText = fmap (showt) $ updated $ edxDyn

    let initialPText = "period goes here"
    let updatedPText = fmap (showt) $ updated $ pDyn

    (valEDText,_, _) <- textWidget 1 (constDyn False) initialEDText updatedEDText -- :: Dyn t Text
    (valPText,_,_) <- textWidget 1 (constDyn False) initialPText updatedPText

    x <- button "calculate"

    let calculateEvent = tagPromptlyDyn (currentValue v) x -- Event t value

    let txChangeEventEDX = tagPromptlyDyn (valEDText) x  -- Event t valEDText
    let txChangeEventP = tagPromptlyDyn (valPText) x

    localChanges <- performEvent $ attachPromptlyDynWith calculateTunning valDyn calculateEvent -- Event t (Tunning)
    edxDyn <- holdDyn 33 $ fmapMaybe ((readMaybe :: String -> Maybe Int) . T.unpack) txChangeEventEDX -- Dynamic t Int
    pDyn <- holdDyn 2 $ fmapMaybe ((readMaybe :: String -> Maybe Int) . T.unpack) txChangeEventP -- Dynamic t Int

    let valDyn = combineitor <$> edxDyn <*> pDyn

    textWithLockAndClipBoardWidget 1 (constDyn True) $ showOutput <$> edxDyn <*> pDyn      

    v <- variable delta localChanges
    return v

showOutput:: Int -> Int -> Text 
showOutput edx p = showt (edx + p)

combineitor:: Int -> Int -> (Int,Int)
combineitor x y = (x,y)

calculateTunning :: MonadIO m => (Int,Int) -> Tuning -> m Tuning
calculateTunning newTar (EdxTuning x p) = return $ EdxTuning (fst newTar) (snd newTar) -- here is where the calculations take place
calculateTunning newTar _ = return $ EdxTuning 0 0

-- cpsWidget::

-- archiveWidget::



-- countDownWidget :: MonadWidget t m => Dynamic t TimerDownState -> W t m (Variable t TimerDownState)
-- countDownWidget deltasDown =  divClass "countDown" $  mdo

--   let initialText = "initial count: 60, change it here"
--   let updatedText = fmap (showt) $ updated timeDyn  -- Event t Text
--   let editable = editableText <$> currentValue v
--   textos <- holdDyn initialText $ leftmost [updatedText, textUpdates]
--   (valTxBx,_) <- textWithLockWidget 1 editable textos
--   let bText = countDownToButtonText <$> currentValue v
--   butt <- dynButton $ bText 
--   let buttonPressedEvent = tagPromptlyDyn valTxBx $ butt
--   let stateWhenButtonPressed = tagPromptlyDyn (currentValue v) buttonPressedEvent
--   localChanges <- performEvent $ attachPromptlyDynWith countDownButtonStateChange timeDyn stateWhenButtonPressed
--   -- this needs to change to attachWith countDownButtonStateChange (current timeDyn) stateWhenButtonPressed, however I have to discover how to updateText in line 81 and keep an eye on the targetTime update issue, for the moment it is clear that buttonPressedEvent caqnnot be in line 81 without consequences in the proper functioning of the widget...

--   timeDyn <- holdDyn 60 $ fmapMaybe ((readMaybe :: String -> Maybe Int) . T.unpack) buttonPressedEvent
--   widgetBuildTime <- liftIO $ getCurrentTime  
--   initialCount <- sample $ current deltasDown
--   let initialTime = countDownToDisplay initialCount widgetBuildTime
--   tick <- tickLossy 0.01 widgetBuildTime 
--   let textUpdates = attachWithMaybe countDownToDisplay (current $ currentValue v) $ fmap _tickInfo_lastUTC tick 
--   v <- returnVariable deltasDown localChanges
--   return v