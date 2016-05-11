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
import           Data.Array
import           Data.Maybe
import           Data.Map
import qualified Data.Text as T

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

initialSound :: Sound
initialSound = simpleSound "sn"

-- Create the sound widget
soundWidget :: R.MonadWidget t m => m (R.Event t Sound)
soundWidget = mdo
  (cont, dynSound) <- elDynAttr' "div" contAttrsDyn $ mdo

    -- Event t (Sound -> Sound)
    upSampleE <- buttonWidget "up" upSAttrs
    incrementSample <- return $ (fmap (\() -> incrementN)) upSampleE

    -- Event t (Sound -> Sound)
    downSampleE <- buttonWidget "down" downSAttrs
    decrementSample <- return $ (fmap (\() -> decrementN)) downSampleE

    -- Event t (Sound -> Sound)
    upRepE <- buttonWidget "up" upRAttrs
    incrementReps <- return $ (fmap (\() -> incrementRepeats)) upRepE

    -- Event t (Sound -> Sound)
    downRepsE <- buttonWidget "down" downRAttrs
    decrementReps <- return $ (fmap (\() -> decrementRepeats)) downRepsE

    -- Event t (Sound -> Sound)
    checkAttrsDyn <- return $ constDyn checkAttrs
    checkE <- checkboxWidget checkAttrsDyn
    setDegradeVal <- return $ (fmap setDegrade) checkE

    -- Event t (Sound -> Sound)
    dropAttrsDyn <- return $ constDyn dropAttrs
    nameE <- dropDownWidget dropAttrsDyn
    updateName <- return $ (fmap rename) nameE

    let soundEvents = [incrementSample, decrementSample, incrementReps, decrementReps, setDegradeVal, updateName]

    -- Dynamic t Sound
    dynamicSound <- foldDyn ($) initialSound (leftmost soundEvents)

    dynamicSoundName <- forDyn dynamicSound show

    -- Display Sound
    display $ dynamicSoundName

    return $ dynamicSound

  -- Event Listeners
  x <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Drop)     (void $ GHCJS.preventDefault)
  y <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Dragover) (void $ GHCJS.preventDefault)
  z <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Dragend)  (void $ GHCJS.preventDefault)
  _ <- R.performEvent_ $ return () <$ y

  let event = leftmost [ClickE    <$ R.domEvent R.Click cont,
                         DragE    <$ R.domEvent R.Drag cont,
                         DragendE <$ x, DropE <$ x]

  soundDyn <- holdDyn Empty event

  -- Set Attributes for container
  contAttrsDyn <- forDyn soundDyn determineSoundAttributes

  let soundE = tag (current dynSound) event

  return $ soundE
  -- Set attributes for container elements
  where
    upSAttrs =   Data.Map.fromList [("class","sample"),("style", "left: 30px; bottom: 80px;")]
    downSAttrs = Data.Map.fromList [("class","sample"),("style", "left: 30px; bottom: 20px;")]
    upRAttrs =   Data.Map.fromList [("class","repeats"),("style", "left: 50px; bottom: 80px;")]
    downRAttrs = Data.Map.fromList [("class","repeats"),("style", "left: 50px; bottom: 20px;")]
    checkAttrs = Data.Map.fromList [("class","checkbox"), ("style", "left: 80px; bottom: 50px;")]
    dropAttrs =  Data.Map.fromList [("class","dropdown"), ("style", "left: 10px; bottom: 21px;")]

determineSoundAttributes :: SoundEvent -> Map String String
determineSoundAttributes soundEvent
        | soundEvent == ClickE     = Data.Map.fromList
            [("draggable", "true"),("class","sound"),("style","background-color: hsl(80,80%,50%); border: 1px solid black;")]
        | soundEvent == DragE      = Data.Map.fromList
            [("draggable", "true"),("class","sound"),("style","background-color: hsl(80,80%,50%); border: 1px solid black;")]
        | soundEvent == DropE      = Data.Map.fromList
            [("draggable", "true"),("class","sound"),("style","background-color: hsl(80,80%,50%); border: 1px solid black;")]
        | soundEvent == DragoverE  = Data.Map.fromList
            [("draggable", "true"),("class","sound"),("style","background-color: hsl(80,80%,30%); border: 1px solid black;")]
        | soundEvent == HoveroverE = Data.Map.fromList
            [("draggable", "true"),("class","sound"),("style","background-color: hsl(80,80%,30%); border: 1px solid black;")]
        | otherwise                = Data.Map.fromList
            [("draggable", "true"),("class","sound"),("style","background-color: hsl(80,80%,50%); border: 1px solid black;")]
