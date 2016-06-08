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


-- Requests should be specific to widgets
data HelperWidgetRequest = HelperRequest (Map String String)

instance Show HelperWidgetRequest where
  show (HelperRequest x) = "helperRequest" ++ (show x)

-------------------------------------------------------------------------------------------------
--                                     Widget Tools                                            --
-------------------------------------------------------------------------------------------------

-- Creates a formattable button
buttonWidget :: R.MonadWidget t m => String -> Map String String -> m (Event t ())
buttonWidget iconType attrs = do
  (button, _) <- elAttr' "a" attrs $ text iconType
  return $ R.domEvent R.Click button

-- Creates a formattable dropdown menu
dropDownWidget :: R.MonadWidget t m => String -> [(String,String)]-> Dynamic t (Map String String) -> m (Event t String)
dropDownWidget initialSample sampleList dynAttrs = do
  let samples = Data.Map.fromList sampleList
  d <- dropdown initialSample (constDyn samples) (def & attributes .~ dynAttrs)
  return $ tagDyn (_dropdown_value d) (_dropdown_change d)

-- Creates a formattable checkbox
checkboxWidget :: R.MonadWidget t m => Bool -> Dynamic t (Map String String) -> m (Event t Bool)
checkboxWidget initialBool dynAttrs = do
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
  return z
  where
    attrs = Data.Map.fromList [("class", "garbage")]
-------------------------------------------------------------------------------------------------
--                                        Widgets                                              --
-------------------------------------------------------------------------------------------------

-- Creates a number picker widget for sample iteration
numberPicker :: MonadWidget t m => Int -> m (Event t Int)
numberPicker initialValue = do

  upSampE <- buttonWidget "+" upAttrs
  incrementNumber <- return $ (fmap (\() -> incrementNum)) upSampE

  downSampE <- buttonWidget "-" downAttrs
  decrementNumber <- return $ (fmap (\() -> decrementNum)) downSampE

  dynamicNum <- foldDyn ($) initialValue $ R.mergeWith (.) [incrementNumber, decrementNumber]

  --dynamicNum <- foldDyn ($) initialValue (leftmost nPickerEvents)

  dynamicString <- forDyn dynamicNum show
  textWidget dynamicString textAttrs
  numEvent <- return $ updated dynamicNum

  return $ numEvent
  where
    upAttrs   = Data.Map.fromList [("class","w3-btn-floating w3-ripple w3-teal")]
    downAttrs = Data.Map.fromList [("class","w3-btn-floating w3-ripple w3-teal")]
    textAttrs = Data.Map.fromList [("class","sampleTxt")]

-- Creates a checkbox widget for degrade value
degradePicker :: MonadWidget t m => Bool -> m (Event t Bool)
degradePicker initialBool = do

  checkAttrsDyn <- return $ constDyn checkAttrs
  eventBool <- checkboxWidget initialBool checkAttrsDyn
  dynamicBool <- holdDyn False eventBool


  -- switch checkAttrs icon depending on bool value
  dynDegString <- forDyn dynamicBool (\b -> (if b then "?" else ""))

  return $ eventBool
  where
    checkAttrs = Data.Map.fromList [("class","checkbox")]

-- Creates a dropdown menu for choosing samples
samplePicker :: MonadWidget t m => String -> m (Event t String)
samplePicker initialSample = do
  let sampleList = [("sn","sn"),("bd","bd"),("arpy","arpy"),("arp","arp"),("hh","hh"),("ht","ht")]
  dropAttrsDyn <- return $ constDyn dropdownAttrs
  sampleE <- dropDownWidget initialSample sampleList dropAttrsDyn

  return $ sampleE
  where
    dropdownAttrs = Data.Map.fromList [("class","w3-dropnav")]
-------------------------------------------------------------------------------------------------
