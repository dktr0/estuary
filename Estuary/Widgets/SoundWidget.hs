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


data Sound = (Maybe (String, Int, Int, Bool))
  deriving (Eq,Show)

initialSound :: Sound
initialSound = ("sn", 1, 1, False)

incrementM :: Sound -> Sound
incrementM (a, b, c, d) = (a, b, c + 1, d)

decrementM :: Sound -> Sound
incrementM (a, b, c, d) = (a, b, c - 1, d)

incrementS :: Sound -> Sound
incrementM (a, b, c, d) = (a, b + 1, c, d)

decrementS :: Sound -> Sound
incrementM (a, b, c, d) = (a, b - 1, c, d)

tDegrade :: Sound -> Sound
tDegrade (a, b, c , d) = (a, b, c, !d)

rename :: String -> Sound -> Sound
rename newName (a, b, c, d) = (newName, b, c, d)

main :: IO ()
main = mainWidget $ do
  elAttr "div" ("style" =: s) $ text "SoundWidget Test"
  soundWidget
  where
    s = "font-size: 50px; margin-left: 155px; font-family: Helvetica; color: steelblue"

-- Create the sound widget
soundWidget :: R.MonadWidget t m => m ()
soundWidget = mdo
  (cont, _) <- elDynAttr' "div" dynAttrs $ do

    -- Event t (Sound -> Sound)
    upSampleE <- buttonWidget "up" upSAttrs
    let incrementSample = (fmap incrementS) upSampleE

    -- Event t (Sound -> Sound)
    downSampleE <- buttonWidget "down" downSAttrs
    let decrementSample = (fmap decrementS) downSampleE

    -- Event t (Sound -> Sound)
    upMultE <- buttonWidget "up" upMAttrs
    let incrementMult = (fmap incrementM) upMultE

    -- Event t (Sound -> Sound)
    downMultE <- buttonWidget "down" downMAttrs
    let decrementMult = (fmap decrementM) downMultE

    -- Event t (Sound -> Sound)
    checkE <- checkboxWidget checkAttrs
    let toggleDegrade = (fmap tDegrade) checkE

    -- Event t (Sound -> Sound)
    nameE <- dropDownWidget dropAttrs
    let updateName = (fmap rename) nameE

    soundEvents <- sequence [incrementSample, decrementSample, incrementMult, decrementMult, toggleDegrade, updateName]

    -- Dynamic t Sound
    dynamicSound <- foldDyn ($) initialState (leftmost soundEvents)

    display $ forDyn dynamicSound (\(a,b,c,d) -> a)

    -- Event Listeners
    x <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Drop)     (void $ GHCJS.preventDefault)
    y <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Dragover) (void $ GHCJS.preventDefault)
    z <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Dragend)  (void $ GHCJS.preventDefault)
    _ <- R.performEvent_ $ return () <$ y
    soundE <- return $ leftmost [ClickE    <$ R.domEvent R.Click cont,
                                  DragE    <$ R.domEvent R.Drag cont,
                                  DragendE <$ x, DropE <$ x]

    -- Set Attributes for container
    contAttrsDyn <- forDyn (holdDyn Empty soundE) determineSoundAttributes

    -- Set attributes for container elements
    where
      upSAttrs =   Data.Map.fromList [("")]
      downSAttrs = Data.Map.fromList [("")]
      upMAttrs =   Data.Map.fromList [("")]
      downMAttrs = Data.Map.fromList [("")]
      checkAttrs = Data.Map.fromList [("")]
      dropAttrs =  Data.Map.fromList [("")]
