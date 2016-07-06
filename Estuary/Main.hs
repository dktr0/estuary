-- Matthew Paine
-- January 28 2016
-- Estuary UI : Reflex/GHCJS front end for Tidal
-- My attempt at creating a drag and drop interface with Reflex and GHCJS
{-# LANGUAGE RecursiveDo, TemplateHaskell #-}

import           Sound.Tidal.Context as Tidal

import           Tidal.Utils
import           Widgets.SoundWidget
import           Widgets.PatternContainer

-- Haskell Imports
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Tuple (fst, snd)
import           Data.String
import           Data.Default
import           Data.Text (Text, intercalate)
import           Data.Array
import           Data.Maybe
import           Data.FileEmbed (embedFile)
import           Data.Map --(Map, fromList, maxView, insert, fold, adjust, delete, findMax, elemAt, partitionWithKey, mapKeys, union, empty)
import qualified Data.Text as T

-- Reflex Imports
-- Reflex Quick Reference                : https://github.com/ryantrinkle/reflex/blob/develop/Quickref.md
--import           Reflex as R
-- Reflex.Dom Quick Reference            : https://github.com/ryantrinkle/reflex-dom/blob/develop/Quickref.md
import           Reflex.Dom as R
import           Reflex.Dom.Internal
-- Reflex.Dom.Widget.Basic Documentation : https://hackage.haskell.org/package/reflex-dom-0.2/docs/Reflex-Dom-Widget-Basic.html#v:DropTag
import           Reflex.Dom.Widget.Basic as R

import           Reflex.Dom.Class as R

-- GHCJS Imports
-- GHCJS Imports
import           GHCJS.Types as GHCJS
import qualified GHCJS.DOM.Event  as GHCJS (IsEvent)
import qualified GHCJS.DOM.Element as GHCJS
import           GHCJS.DOM.EventM as GHCJS (preventDefault, stopPropagation, EventM)

main :: IO ()
main = mainWidgetWithCss $(Data.FileEmbed.embedFile "../static/css/helperwidgets.css") estuaryApp

estuaryApp :: MonadWidget t m => m ()
estuaryApp = do
  el "div" $ do
    elAttr "section" ("class" =: "soundcontainer") $ do
      mainHeader
      patternContainerWidget
      return ()
    infoFooter

mainHeader :: MonadWidget t m => m ()
mainHeader = el "h1" $ text "Estuary Test"

-- | Display static information about the application
infoFooter :: MonadWidget t m => m ()
infoFooter = do
  elAttr "footer" ("class" =: "info") $ do
    el "p" $ do
      text "Written by "
      elAttr "a" ("href" =: "https://github.com/Moskau") $ text "Matthew Paine"
      text " and "
      elAttr "a" ("href" =: "https://github.com/d0kt0r0") $ text "David Ogborn"
