-- Matthew Paine
-- January 28 2016
-- Estuary UI : Reflex/GHCJS front end for Tidal
-- My attempt at creating a drag and drop interface with Reflex and GHCJS
{-# LANGUAGE RecursiveDo #-}

module           Estuary.Widgets.SoundWidget where

import           Sound.Tidal.Context as Tidal
import           Tidal.Utils
import           Widgets.HelperWidgets
import           Types.Sound
import           Types.SoundPattern

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

initialContainer :: SoundContainer
initialContainer = newSoundContainer

main :: IO ()
main = mainWidget $ do
  elAttr "div" ("style" =: s) $ text "SoundWidget Test"
  patternContainerWidget
  where
    s = "font-size: 50px; margin-left: 155px; font-family: Helvetica; color: steelblue"

-- Create the sound widget
patternContainerWidget :: R.MonadWidget t m => m ()
patternContainerWidget = mdo
  (cont, _) <- elDynAttr' "div" contAttrsDyn $ do
    -- Create new block (do for each)
    feedback <- listWithKey allSounds $ \k oneSound -> mdo

      -- Create a sampleBlock element
      (a,b) <- soundWidget

      -- concatonate block names
      return (a,b)

    addSoundE <- buttonWidget "add" appendAttrs
    let addSound = (fmap appendSound) addSoundE

    remSoundE <- -- Some soundWidget event
    let remSound = (fmap removeSound) remSoundE

    insSoundE <- -- Some soundWidget event
    let insSound = (fmap insertSound) insSoundE

    allSounds <- foldDyn ($) initial (leftmost $ [addSound, remSound, insSound])

  patternContE <- return $ leftmost [ClickE   <$ R.domEvent R.Click cont,
                                     DragE    <$ R.domEvent R.Drag cont,
                                     DragendE <$ x, DropE <$ x]
  contAttrsDyn <- determineContAttributes Empty
  return ()
  where
    appendAttrs = Data.Map.fromList [("class","squarebutton"),("style","left: 20px; bottom: 20px;")]

determineContAttributes :: SoundEvent -> Map String String
determineContAttributes soundEvent
        | soundEvent == ClickE     = Data.Map.fromList
            [("class","soundcontainer"),("style","background-color: hsl(80,80%,30%); border: 3px solid black;")]
        | soundEvent == DragE      = Data.Map.fromList
            [("class","soundcontainer"),("style","background-color: hsl(80,80%,50%); border: 1px solid black;")]
        | soundEvent == DropE      = Data.Map.fromList
            [("class","soundcontainer"),("style","background-color: hsl(80,80%,50%); border: 1px solid black;")]
        | soundEvent == DragoverE  = Data.Map.fromList
            [("class","soundcontainer"),("style","background-color: hsl(80,80%,30%); border: 1px solid black;")]
        | soundEvent == HoveroverE = Data.Map.fromList
            [("class","soundcontainer"),("style","background-color: hsl(80,80%,30%); border: 1px solid black;")]
        | otherwise                = Data.Map.fromList
            [("class","soundcontainer"),("style","background-color: hsl(80,80%,50%); border: 1px solid black;")]
