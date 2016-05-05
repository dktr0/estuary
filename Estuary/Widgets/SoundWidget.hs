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

main :: IO ()
main = mainWidget $ do
  elAttr "div" ("style" =: s) $ text "Estuary"
  blockWidget
  where
    s = "font-size: 50px; margin-left: 155px; font-family: Helvetica; color: steelblue"

-- Create the sound widget
soundWidget :: R.MonadWidget t m => m ()
soundWidget = mdo
  (cont, _) <- elDynAttr' "div" dynAttrs $ do

    -- UpSample
    upSample <- buttonWidget "up"
      -- Increment sample counter on click
      -- update Sound

    -- DownSample
    downSample <- buttonWidget "down"
      -- Decrement sample counter on click
      -- update Sound

    -- UpMult
    upMult <- buttonWidget "up"
      -- increment mult counter on click
      -- update Sound

    -- DownMult
    downMult <- buttonWidget "down"
      -- decrement mult counter on click
      -- update Sound

    -- Degrade?
    check <- checkboxWidget
      -- toggle bool
      -- update Sound

    -- Name
    name <- dropDownWidget
      -- update Sound

    -- Set Attributes
    attrsDyn <- determineBoxAttributes Empty

    -- Event Listeners
    x <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Drop)     (void $ GHCJS.preventDefault)
    y <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Dragover) (void $ GHCJS.preventDefault)
    z <- R.wrapDomEvent (R._el_element cont) (R.onEventName R.Dragend)  (void $ GHCJS.preventDefault)
    _ <- R.performEvent_ $ return () <$ y
    boxDomE <- return $ leftmost [ClickE   <$ R.domEvent R.Click cont,
                                  DragE    <$ R.domEvent R.Drag cont,
                                  DragendE <$ x, DropE <$ x]
