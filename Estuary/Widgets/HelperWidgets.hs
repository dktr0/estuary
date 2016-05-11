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

--------------------------------------- Widget Tools -------------------------------------------
-- Input positional information relative to container
buttonWidget :: R.MonadWidget t m => String -> Map String String -> m (Event t ())
buttonWidget iconType attrs = do
  (button, _) <- elAttr' "div" attrs $ text iconType
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
-------------------------------------------------------------------------------------------------
