-- Matthew Paine
-- January 28 2016
-- Estuary UI : Reflex/GHCJS front end for Tidal
-- My attempt at creating a drag and drop interface with Reflex and GHCJS
{-# LANGUAGE RecursiveDo #-}


module Estuary.Widgets.HelperWidgets where
-- Reflex.Dom Quick Reference            : https://github.com/ryantrinkle/reflex-dom/blob/develop/Quickref.md
import           Reflex.Dom as R
-- Reflex.Dom.Widget.Basic Documentation : https://hackage.haskell.org/package/reflex-dom-0.2/docs/Reflex-Dom-Widget-Basic.html#v:DropTag
import           Reflex.Dom.Widget.Basic as R

import           Reflex.Dom.Class as R

--------------------------------------- Widget Tools -------------------------------------------
-- Input positional information relative to container
buttonWidget :: R.MonadWidget t m => String -> Map String String -> m (Event t ())
buttonWidget iconType attrs = do
  (button, _) <- elAttr' "div" attrs $ icon iconType
  return $ R.domEvent R.Click button

-- Input positional information relative to container
dropDownWidget :: R.MonadWidget t m => Dynamic t (Map String String) -> m (Dynamic t String)
dropDownWidget dynAttrs = do
  let samples = [("bd","bd"),("sn","sn"),("arpy","arpy"),("arp","arp"),("hh","hh"),("ht","ht")]
  d <- dropdown "bd" (constDyn samples) def & _dropdownConfig_attributes .~ dynAttrs
  return $ _dropdown_value d

-- Input positional information relative to container
checkboxWidget :: R.MonadWidget t m => m (Dynamic t Bool)
checkboxWidget = do
  return (constDyn False)

determineBoxAttributes :: BoxEvent -> Map String String
determineBoxAttributes boxEvent
        | boxEvent == ClickE = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width:30px; background-color: hsl(80,80%,30%);" ++
              "height: 30px; float: left; border: 3px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em;")]
        | boxEvent == DragE = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width:30px; background-color: hsl(80,80%,50%);" ++
              "height: 30px; float: left; border: 1px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em; left:")]
        | boxEvent == DropE = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width: 30px; background-color: hsl(80,80%,50%);" ++
              "height: 30px; float: left; border: 1px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em; left:")]
        | boxEvent == DragoverE = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width:30px; background-color: hsl(80,80%,30%);" ++
              "height: 30px; float: left; border: 1px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em; left:")]
        | boxEvent == HoveroverE = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width:30px; background-color: hsl(80,80%,30%);" ++
              "height: 30px; float: left; border: 1px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em; left:")]
        | otherwise            = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width: 30px; background-color: hsl(80,80%,50%);" ++
              "height: 30px; float: left; border: 1px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em; left:")]
-------------------------------------------------------------------------------------------------
