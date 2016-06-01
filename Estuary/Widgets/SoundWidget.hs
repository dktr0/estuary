-- Matthew Paine
-- January 28 2016
-- Estuary UI : Reflex/GHCJS front end for Tidal
-- My attempt at creating a drag and drop interface with Reflex and GHCJS
{-# LANGUAGE RecursiveDo #-}

module           Widgets.SoundWidget where

import           Sound.Tidal.Context as Tidal
import           Tidal.Utils
import           Widgets.HelperWidgets
import           Types.Sound

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

-- Create the sound widget
soundWidget :: R.MonadWidget t m => Dynamic t Sound -> m (R.Event t (SoundEvent, Sound))
soundWidget sound = mdo
  (cont, dynSound) <- elDynAttr' "div" contAttrsDyn $ mdo

    soundBeh <- return $ current sound
    soundConst <- sample soundBeh
    let test = constDyn soundConst
    display test

    -- Create n picker widget
    changeN <- nPicker
    -- Create repeats changer widget
    changeReps <- repeatsPicker
    -- Create degrade changer widget
    changeDegrade <- degradePicker
    -- Crate sample changer widget
    changeSamples <- samplePicker

    -- Create list of events
    let soundEvents = [changeN, changeReps, changeDegrade, changeSamples]

    -- Fold events into dynamic sound
    dynamicSound <- foldDyn ($) soundConst (leftmost soundEvents)

    -- Get dynamic sound name string
    dynamicSoundName <- forDyn dynamicSound show

    -- Display dynamic sound name
    display $ sound

    return $ dynamicSound

  -- Event Listeners
  x <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Drop)     (void $ GHCJS.preventDefault)
  y <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Dragover) (void $ GHCJS.preventDefault)
  z <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Dragend)  (void $ GHCJS.preventDefault)
  _ <- R.performEvent_ $ return () <$ y

  let event = leftmost [ DragE      <$ R.domEvent R.Drag cont,
                         ClickE     <$ R.domEvent R.Click cont,
                         DragendE   <$ z, DropE <$ x]

  soundDyn <- holdDyn Empty event

  -- Set Attributes for container
  contAttrsDyn <- forDyn soundDyn determineSoundAttributes

  let soundE = tag (current dynSound) event

  let soundTupleE = attachDyn soundDyn soundE

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
