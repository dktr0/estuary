-- Matthew Paine
-- January 28 2016
-- Estuary UI : Reflex/GHCJS front end for Tidal
-- My attempt at creating a drag and drop interface with Reflex and GHCJS
{-# LANGUAGE RecursiveDo #-}

module Widgets.HelperWidgets where

import           Types.Sound

-- Haskell Imports
import           Control.Monad
import           Data.Tuple (fst, snd)
import           Data.String
import           Data.Default
import           Data.Maybe
import           Data.Map

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

-------------------------------------------------------------------------------------------------
--                                     Widget Tools                                            --
-------------------------------------------------------------------------------------------------

-- Creates a formattable button
buttonWidget :: R.MonadWidget t m => String -> Map String String -> m (Event t ())
buttonWidget iconType attrs = do
  (button, _) <- elAttr' "a" attrs $ text iconType
  return $ R.domEvent R.Click button

-- Creates a formattable dropdown menu
dropDownWidget :: R.MonadWidget t m => Dynamic t (Map String String) -> m (Event t String)
dropDownWidget dynAttrs = do
  let samples = Data.Map.fromList [("sn","sn"),("bd","bd"),("arpy","arpy"),("arp","arp"),("hh","hh"),("ht","ht")]
  d <- dropdown "sn" (constDyn samples) (def & attributes .~ dynAttrs)
  return $ tagDyn (_dropdown_value d) (_dropdown_change d)

-- Creates a formattable checkbox
checkboxWidget :: R.MonadWidget t m => Dynamic t (Map String String) -> m (Event t Bool)
checkboxWidget dynAttrs = do
  c <- checkbox False (def & attributes .~ dynAttrs)
  return $ (_checkbox_change c)

-- Creates formattable text
textWidget :: R.MonadWidget t m => Dynamic t String -> Map String String -> m ()
textWidget dynName attrs = do
  (el, _) <- elAttr' "p" attrs $ display dynName
  return ()

garbageWidget :: R.MonadWidget t m => m (Event t ())
garbageWidget = do
  (el, _) <- elAttr' "div" attrs $ text "garbage"
  x <- R.wrapDomEvent (R._el_element el) (R.onEventName R.Drop)     (void $ GHCJS.preventDefault)
  y <- R.wrapDomEvent (R._el_element el) (R.onEventName R.Dragover) (void $ GHCJS.preventDefault)
  z <- R.wrapDomEvent (R._el_element el) (R.onEventName R.Dragend)  (void $ GHCJS.preventDefault)
  _ <- R.performEvent_ $ return () <$ y
  return x
  where
    attrs = Data.Map.fromList [("class", "garbage")]
-------------------------------------------------------------------------------------------------
--                                        Widgets                                              --
-------------------------------------------------------------------------------------------------

-- Creates a number picker widget for sample iteration
nPicker :: MonadWidget t m => m (Event t (Sound -> Sound))
nPicker = do
  upSampE <- buttonWidget "+" upAttrs
  incrementSample <- return $ (fmap (\() -> incrementN)) upSampE

  downSampE <- buttonWidget "-" downAttrs
  decrementSample <- return $ (fmap (\() -> decrementN)) downSampE

  let nPickerEvents = [incrementSample, decrementSample]
  let sampsE = leftmost nPickerEvents

  dynamicSound <- foldDyn ($) initialSound (leftmost nPickerEvents)

  dynN <- forDyn dynamicSound n
  dynNString <- forDyn dynN show
  textWidget dynNString textAttrs
  return $ sampsE
  where
    upAttrs   = Data.Map.fromList [("class","w3-btn-floating w3-ripple w3-teal")]
    downAttrs = Data.Map.fromList [("class","w3-btn-floating w3-ripple w3-teal")]
    textAttrs = Data.Map.fromList [("class","sampleTxt")]

-- Creates a number picker widget for pattern repeats
repeatsPicker :: MonadWidget t m => m (Event t (Sound -> Sound))
repeatsPicker = do
  upRepsE <- buttonWidget "+" upAttrs
  incrementReps <- return $ (fmap (\() -> incrementRepeats)) upRepsE

  downRepsE <- buttonWidget "-" downAttrs
  decrementReps <- return $ (fmap (\() -> decrementRepeats)) downRepsE

  let repeatsPickerEvents = [incrementReps, decrementReps]
  let repsE = leftmost repeatsPickerEvents

  dynamicSound <- foldDyn ($) initialSound (leftmost repeatsPickerEvents)

  dynRepeats <- forDyn dynamicSound repeats
  dynRepeatsString <- forDyn dynRepeats show
  textWidget dynRepeatsString textAttrs

  return $ repsE
  where
    upAttrs   = Data.Map.fromList [("class","w3-btn-floating w3-ripple w3-teal")]
    downAttrs = Data.Map.fromList [("class","w3-btn-floating w3-ripple w3-teal")]
    textAttrs = Data.Map.fromList [("class","sampleTxt")]

-- Creates a checkbox widget for degrade value
degradePicker :: MonadWidget t m => m (Event t (Sound -> Sound))
degradePicker = do
  checkAttrsDyn <- return $ constDyn checkAttrs
  checkE <- checkboxWidget checkAttrsDyn
  degE <- return $ (fmap setDegrade) checkE

  dynCheck <- holdDyn False checkE
  dynDegString <- forDyn dynCheck (\b -> (if b then "?" else ""))
  textWidget dynDegString textAttrs

  return $ degE
  where
    checkAttrs = Data.Map.fromList [("class","checkbox")]
    textAttrs = Data.Map.fromList [("class","sampleTxt")]

-- Creates a dropdown menu for choosing samples
samplePicker :: MonadWidget t m => m (Event t (Sound -> Sound))
samplePicker = do
  dropAttrsDyn <- return $ constDyn dropdownAttrs
  dropdownE <- dropDownWidget dropAttrsDyn
  nameE <- return $ (fmap rename) dropdownE

  dynName <- holdDyn "sn" dropdownE
  textWidget dynName textAttrs

  return $ nameE
  where
    dropdownAttrs = Data.Map.fromList [("class","w3-dropnav")]
    textAttrs = Data.Map.fromList [("class","sampleTxt")]
-------------------------------------------------------------------------------------------------


-- To do
-- Use text widget
-- Format widgets using css file
-- Work on pattern container
