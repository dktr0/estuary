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
import           Data.Monoid

-- Reflex.Dom Quick Reference            : https://github.com/ryantrinkle/reflex-dom/blob/develop/Quickref.md
import           Reflex.Dom as R
-- Reflex.Dom.Widget.Basic Documentation : https://hackage.haskell.org/package/reflex-dom-0.2/docs/Reflex-Dom-Widget-Basic.html#v:DropTag
import           Reflex.Dom.Widget.Basic as R

import           Reflex.Dom.Class as R

data SoundEvent = ClickE | DragE | DropE | DragoverE | DragendE | HoveroverE | Empty
  deriving (Eq, Show)

initialSound :: Sound
initialSound = simpleSound "sn"

--------------------------------------- Widget Tools -------------------------------------------
-- Input positional information relative to container
buttonWidget :: R.MonadWidget t m => String -> Map String String -> m (Event t ())
buttonWidget iconType attrs = do
  (button, _) <- elAttr' "button" attrs $ text iconType
  return $ R.domEvent R.Click button

-- Input positional information relative to container
dropDownWidget :: R.MonadWidget t m => Dynamic t (Map String String) -> m (Event t String)
dropDownWidget dynAttrs = do
  let samples = Data.Map.fromList [("bd","bd"),("sn","sn"),("arpy","arpy"),("arp","arp"),("hh","hh"),("ht","ht")]
  d <- dropdown "bd" (constDyn samples) (def & attributes .~ dynAttrs)
  return $ tagDyn (_dropdown_value d) (_dropdown_change d)

-- Input positional information relative to container
checkboxWidget :: R.MonadWidget t m => Dynamic t (Map String String) -> m (Event t Bool)
checkboxWidget dynAttrs = do
  c <- checkbox False (def & attributes .~ dynAttrs)
  return $ (_checkbox_change c)


textWidget :: R.MonadWidget t m => Dynamic t String -> Map String String -> m ()
textWidget dynName attrs = do
  (el, _) <- elAttr' "p" attrs $ display dynName
  return ()


nPicker :: MonadWidget t m => m (Event t (Sound -> Sound))
nPicker = do
  upSampE <- buttonWidget "up" upAttrs
  incrementSample <- return $ (fmap (\() -> incrementN)) upSampE

  downSampE <- buttonWidget "down" downAttrs
  decrementSample <- return $ (fmap (\() -> decrementN)) downSampE

  samps <- (<>) incrementSample decrementSample

  let nPickerEvents = [incrementSample, decrementSample]

  dynamicSound <- foldDyn ($) initialSound (leftmost nPickerEvents)

  -- Display text (attrs)
  dynN <- forDyn dynamicSound n
  display dynN

  return $ samps
  where
    upAttrs   = Data.Map.fromList [("class","sample"),("style", "left: 30px; bottom: 80px;")]
    downAttrs = Data.Map.fromList [("class","sample"),("style", "left: 30px; bottom: 20px;")]

repeatsPicker :: MonadWidget t m => m (Event t (Sound -> Sound))
repeatsPicker = do
  upRepsE <- buttonWidget "up" upAttrs
  incrementReps <- return $ (fmap (\() -> incrementRepeats)) upRepsE

  downRepsE <- buttonWidget "down" downAttrs
  decrementReps <- return $ (fmap (\() -> decrementRepeats)) downRepsE

  reps <- (<>) incrementReps decrementReps

  let repeatsPickerEvents = [incrementReps, decrementReps]

  dynamicSound <- foldDyn ($) initialSound (leftmost repeatsPickerEvents)

  -- Display text (attrs)
  dynRepeats <- forDyn dynamicSound repeats
  display dynRepeats

  let doot = _

  return $ reps

  where
    upAttrs   = Data.Map.fromList [("class","repeats"),("style", "left: 50px; bottom: 80px;")]
    downAttrs = Data.Map.fromList [("class","repeats"),("style", "left: 50px; bottom: 20px;")]

degradePicker :: MonadWidget t m => m (Event t (Sound -> Sound))
degradePicker = do
  checkAttrsDyn <- return $ constDyn checkAttrs
  checkE <- checkboxWidget checkAttrsDyn
  deg <- return $ (fmap setDegrade) checkE
  return $ deg
  where
    checkAttrs = Data.Map.fromList [("class","checkbox"), ("style", "left: 80px; bottom: 50px;")]

samplePicker :: MonadWidget t m => m (Event t (Sound -> Sound))
samplePicker = do
  dropAttrsDyn <- return $ constDyn dropAttrs
  nameE <- dropDownWidget dropAttrsDyn
  updateName <- return $ (fmap rename) nameE

  -- Display name (attrs)
  return $ nameE
  where
    dropAttrs =  Data.Map.fromList [("class","dropdown"), ("style", "left: 10px; bottom: 21px;")]



-------------------------------------------------------------------------------------------------
