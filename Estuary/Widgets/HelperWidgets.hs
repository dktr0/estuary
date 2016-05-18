-- Matthew Paine
-- January 28 2016
-- Estuary UI : Reflex/GHCJS front end for Tidal
-- My attempt at creating a drag and drop interface with Reflex and GHCJS
{-# LANGUAGE RecursiveDo #-}

module Widgets.HelperWidgets where

import           Types.Sound

-- Haskell Imports
import           Data.Default
import           Data.Map

-- Reflex.Dom Quick Reference            : https://github.com/ryantrinkle/reflex-dom/blob/develop/Quickref.md
import           Reflex.Dom as R
-- Reflex.Dom.Widget.Basic Documentation : https://hackage.haskell.org/package/reflex-dom-0.2/docs/Reflex-Dom-Widget-Basic.html#v:DropTag
import           Reflex.Dom.Widget.Basic as R

import           Reflex.Dom.Class as R

-------------------------------------------------------------------------------------------------
--                                     Widget Tools                                            --
-------------------------------------------------------------------------------------------------

-- Creates a formattable button
buttonWidget :: R.MonadWidget t m => String -> Map String String -> m (Event t ())
buttonWidget iconType attrs = do
  (button, _) <- elAttr' "button" attrs $ text iconType
  return $ R.domEvent R.Click button

-- Creates a formattable dropdown menu
dropDownWidget :: R.MonadWidget t m => Dynamic t (Map String String) -> m (Event t String)
dropDownWidget dynAttrs = do
  let samples = Data.Map.fromList [("bd","bd"),("sn","sn"),("arpy","arpy"),("arp","arp"),("hh","hh"),("ht","ht")]
  d <- dropdown "bd" (constDyn samples) (def & attributes .~ dynAttrs)
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

-------------------------------------------------------------------------------------------------
--                                        Widgets                                              --
-------------------------------------------------------------------------------------------------

-- Creates a number picker widget for sample iteration
nPicker :: MonadWidget t m => m (Event t (Sound -> Sound))
nPicker = do
  upSampE <- buttonWidget "up" upAttrs
  incrementSample <- return $ (fmap (\() -> incrementN)) upSampE

  downSampE <- buttonWidget "down" downAttrs
  decrementSample <- return $ (fmap (\() -> decrementN)) downSampE

  let nPickerEvents = [incrementSample, decrementSample]
  let sampsE = leftmost nPickerEvents

  dynamicSound <- foldDyn ($) initialSound (leftmost nPickerEvents)

  dynN <- forDyn dynamicSound n
  dynNString <- forDyn dynN show
  textWidget dynNString textAttrs
  return $ sampsE
  where
    upAttrs   = Data.Map.fromList [("class","sampleBtn"),("style", "left: 30px; bottom: 80px;")]
    downAttrs = Data.Map.fromList [("class","sampleBtn"),("style", "left: 30px; bottom: 20px;")]
    textAttrs = Data.Map.fromList [("class","sampleTxt"),("style","left: 30px; bottom: 50px;")]

-- Creates a number picker widget for pattern repeats
repeatsPicker :: MonadWidget t m => m (Event t (Sound -> Sound))
repeatsPicker = do
  upRepsE <- buttonWidget "up" upAttrs
  incrementReps <- return $ (fmap (\() -> incrementRepeats)) upRepsE

  downRepsE <- buttonWidget "down" downAttrs
  decrementReps <- return $ (fmap (\() -> decrementRepeats)) downRepsE

  let repeatsPickerEvents = [incrementReps, decrementReps]
  let repsE = leftmost repeatsPickerEvents

  dynamicSound <- foldDyn ($) initialSound (leftmost repeatsPickerEvents)

  dynRepeats <- forDyn dynamicSound repeats
  dynRepeatsString <- forDyn dynRepeats show
  textWidget dynRepeatsString textAttrs

  return $ repsE
  where
    upAttrs   = Data.Map.fromList [("class","repeatsBtn"),("style", "left: 50px; bottom: 80px;")]
    downAttrs = Data.Map.fromList [("class","repeatsBtn"),("style", "left: 50px; bottom: 20px;")]
    textAttrs = Data.Map.fromList [("class","sampleTxt"),("style", "left: 50px; bottom: 50px;")]

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
    checkAttrs = Data.Map.fromList [("class","checkbox"), ("style", "left: 80px; bottom: 50px;")]
    textAttrs = Data.Map.fromList [("class","sampleTxt"), ("style", "left: 80px; bottom: 60px;")]

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
    dropdownAttrs = Data.Map.fromList [("class","dropdown"), ("style", "left: 10px; bottom: 20px;")]
    textAttrs = Data.Map.fromList [("class","sampleTxt"), ("style", "left 10px; bottom: 10px;")]
-------------------------------------------------------------------------------------------------


-- To do
-- Use text widget
-- Format widgets using css file
-- Work on pattern container
