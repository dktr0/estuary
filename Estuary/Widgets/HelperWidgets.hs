-- Matthew Paine
-- January 28 2016
-- Estuary UI : Reflex/GHCJS front end for Tidal
-- My attempt at creating a drag and drop interface with Reflex and GHCJS
{-# LANGUAGE RecursiveDo #-}

module Widgets.HelperWidgets where

-- Haskell Imports
import           Data.Default
import           Data.Map

-- Reflex.Dom Quick Reference            : https://github.com/ryantrinkle/reflex-dom/blob/develop/Quickref.md
import           Reflex.Dom as R
-- Reflex.Dom.Widget.Basic Documentation : https://hackage.haskell.org/package/reflex-dom-0.2/docs/Reflex-Dom-Widget-Basic.html#v:DropTag
import           Reflex.Dom.Widget.Basic as R

import           Reflex.Dom.Class as R

data SoundEvent = ClickE | DragE | DropE | DragoverE | DragendE | HoveroverE | Empty
  deriving (Eq, Show)

initialSound :: Sound
initialCounter = simpleSound "sn"

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

nPicker :: MonadWidget t m => m (Event t (Sound -> Sound))
numberPicker = do
  (cont,sampsE) <- elDynAttr' "div" contAttrsDyn $ do

    upSampE <- buttonWidget "up" upRAttrs
    incrementSample <- return $ (fmap (\() -> incrementN)) upSampE

    downSampE <- buttonWidget "down" downRAttrs
    decrementSample <- return $ (fmap (\() -> decrementN)) downSampE

    sampsE <- <> incrementSample decrementSample

    let samplePickerEvents = [incrementSample, decrementSample]

    dynamicSound <- foldDyn ($) initialCounter (nPickerEvents)

    -- Display text (attrs)

    return $ samps

  return $ sampsE
  where
    upAttrs = Data.Map.fromList [("")]
    downAttrs = Data.Map.fromList [("")]
    contAttrs = Data.Map.fromList [("")]

repeatsPicker :: MonadWidget t m => m (Event t (Sound -> Sound))
repeatsPicker = do
  (cont, repsE) <- elDynAttr' "div" contAttrsDyn $ do

    upRepsE <- buttonWidget "up" upRAttrs
    incrementReps <- return $ (fmap (\() -> incrementRepeats)) upRepE

    downRepsE <- buttonWidget "down" downRAttrs
    decrementReps <- return $ (fmap (\() -> decrementRepeats)) downRepsE

    repsE <- <> $ incrementReps decrementReps

    let repeatsPickerEvents = [incrementReps, decrementReps]

    dynamicSound <- foldDyn ($) initialSound (repeatsPickerEvents)

    -- Display text (attrs)

    return $ reps

  return $ repsE
  where
    upAttrs = Data.Map.fromList [("")]
    downAttrs = Data.Map.fromList [("")]
    contAttrs = Data.Map.fromList [("")]

degradePicker :: MonadWidget t m => m (Event t (Sound -> Sound))
degradePicker = do
  (cont, degE) <- elDynAttr' "div" contAttrsDyn $ do
    checkAttrsDyn <- return $ constDyn checkAttrs
    checkE <- checkboxWidget checkAttrsDyn
    deg <- return $ (fmap setDegrade) checkE
    return $ deg

    -- display degrade (attrs)

  return $ degE
  where
    contAttrs = Data.Map.fromList [("")]
    checkAttrs = Data.Map.fromList [("")]

samplePicker :: MonadWidget t m => m (Event t (Sound -> Sound))
samplePicker = do
  (cont, nameE) <- elDynAttr' "div" contAttrsDyn $ do
    dropAttrsDyn <- return $ constDyn dropAttrs
    nameE <- dropDownWidget dropAttrsDyn
    updateName <- return $ (fmap rename) nameE

    -- Display name (attrs)

    return $ updateName
  return $ nameE
-------------------------------------------------------------------------------------------------
