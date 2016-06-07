-- Matthew Paine
-- January 28 2016
-- Estuary UI : Reflex/GHCJS front end for Tidal
-- My attempt at creating a drag and drop interface with Reflex and GHCJS
{-# LANGUAGE RecursiveDo #-}

module           Widgets.SoundWidget where

import           Sound.Tidal.Context as Tidal
import           Tidal.Utils
import           Widgets.HelperWidgets
import           Types.Sound as S

-- Haskell Imports
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Tuple (fst, snd)
import           Data.String
import           Data.Default
import           Data.Maybe
import           Data.Map

-- Reflex Imports
-- Reflex Quick Reference                : https://github.com/ryantrinkle/reflex/blob/develop/Quickref.md
--import           Reflex as R
-- Reflex.Dom Quick Reference            : https://github.com/ryantrinkle/reflex-dom/blob/develop/Quickref.md
import           Reflex.Dom as R
-- Reflex.Dom.Widget.Basic Documentation : https://hackage.haskell.org/package/reflex-dom-0.2/docs/Reflex-Dom-Widget-Basic.html#v:DropTag
import           Reflex.Dom.Widget.Basic as R

import           Reflex.Dom.Class as R

-- GHCJS Imports
import           GHCJS.Types as GHCJS
import qualified GHCJS.DOM.Event  as GHCJS (IsEvent)
import qualified GHCJS.DOM.Element as GHCJS
import           GHCJS.DOM.EventM as GHCJS (preventDefault, stopPropagation, EventM)

data SoundWidgetRequest = CommonRequest (Map String String) |
                          ScrambleYourself |
                          Pulse (Map String String) |
                          BecomeSound Sound

instance Show SoundWidgetRequest where
  show (CommonRequest x) = "commonRequest" ++ (show x)
  show (ScrambleYourself) = "scramble"
  show (Pulse x) = "pulse" ++ (show x)
  show (BecomeSound x) = "become" ++ (show x)

-- Create the sound widget
soundWidget :: R.MonadWidget t m => Sound -> R.Event t SoundWidgetRequest -> m (R.Event t (SoundEvent, Sound))
soundWidget initSound widgetRequest = mdo
  (cont, dynSound) <- elDynAttr' "div" contAttrsDyn $ mdo

    -- Create n picker widget
    numEvent <- numberPicker (S.n initSound) never
    changeN <- return $ fmap setN numEvent
    -- Create repeats changer widget
    repEvent <- numberPicker (S.repeats initSound) never
    changeReps <- return $ fmap setReps repEvent
    -- Create degrade changer widget
    degEvent <- degradePicker (S.degraded initSound) never
    changeDeg <- return $ fmap setDegrade degEvent
    -- Crate sample changer widget
    sampEvent <- samplePicker (S.name initSound) never
    changeSamp <- return $ fmap setSamp sampEvent

    -- Create list of events
    let soundEvents = [changeN, changeReps, changeDeg, changeSamp]

    -- Fold events into dynamic sound
    dynamicSound <- foldDyn ($) initSound (leftmost soundEvents)

    -- Get dynamic sound name string
    dynamicSoundName <- forDyn dynamicSound show

    display dynamicSoundName

    return $ dynamicSound

  -- Event Listeners
  x <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Drop)     (void $ GHCJS.preventDefault)
  y <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Dragover) (void $ GHCJS.preventDefault)
  z <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Dragend)  (void $ GHCJS.preventDefault)
  _ <- R.performEvent_ $ return () <$ y

  let event = leftmost [ DragE      <$ R.domEvent R.Drag cont,
                         ClickE     <$ R.domEvent R.Click cont,
                         DragendE   <$ z, DropE <$ x]

  soundEventDyn <- holdDyn Empty event

  -- Set Attributes for container
  contAttrsDyn <- forDyn soundEventDyn determineSoundAttributes

  let soundE = tag (current dynSound) event

  let soundTupleE = attachDyn soundEventDyn soundE

  return $ soundTupleE

determineSoundAttributes :: SoundEvent -> Map String String
determineSoundAttributes soundEvent
        | soundEvent == ClickE     = Data.Map.fromList
            [("draggable", "true"),("class","w3-container w3-light-grey w3-border-dark-grey")]
        | soundEvent == DragE      = Data.Map.fromList
            [("draggable", "true"),("class","w3-container w3-light-grey w3-border-dark-grey")]
        | soundEvent == DropE      = Data.Map.fromList
            [("draggable", "true"),("class","w3-container w3-light-grey w3-border-dark-grey")]
        | soundEvent == DragoverE  = Data.Map.fromList
            [("draggable", "true"),("class","w3-container w3-light-grey w3-border-dark-grey")]
        | soundEvent == HoveroverE = Data.Map.fromList
            [("draggable", "true"),("class","w3-container w3-light-grey w3-border-dark-grey")]
        | otherwise                = Data.Map.fromList
            [("draggable", "true"),("class","w3-container w3-light-grey w3-border-dark-grey")]
