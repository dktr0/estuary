-- Matthew Paine
-- January 28 2016
-- Estuary UI : Reflex/GHCJS front end for Tidal
-- My attempt at creating a drag and drop interface with Reflex and GHCJS
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ExistentialQuantification #-}

-- Haskell Imports
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Text (Text, intercalate)
import           Data.Array
import           Data.Map (Map, fromList, maxView, insert)
import qualified Data.Text as T

-- Reflex Imports
-- Reflex Quick Reference                : https://github.com/ryantrinkle/reflex/blob/develop/Quickref.md
import           Reflex as R
-- Reflex.Dom Quick Reference            : https://github.com/ryantrinkle/reflex-dom/blob/develop/Quickref.md
import           Reflex.Dom as R
-- Reflex.Dom.Widget.Basic Documentation : https://hackage.haskell.org/package/reflex-dom-0.2/docs/Reflex-Dom-Widget-Basic.html#v:DropTag
import           Reflex.Dom.Widget.Basic as R

import           Reflex.Dom.Class as R

-- GHCJS Imports
import           GHCJS.Types as GHCJS
import           GHCJS.DOM.Event  as GHCJS
import qualified GHCJS.DOM.Element as GHCJS
import qualified GHCJS.DOM.EventM as GHCJS

data DragEvent = DragEnter | DragExit | DragDrop | Empty
  deriving (Eq, Show)

stopAll :: GHCJS.IsEvent event => GHCJS.EventM event e ()
stopAll = do
  GHCJS.preventDefault
  GHCJS.stopPropagation
  return()

main :: IO ()
main = mainWidget $ do
  elAttr "div" ("style" =: s) $ text "Estuary"
  setup
  blockName <- sampleWidget
  sampleWidgetContainer blockName
  text ""
  where
    s = "font-size: 50px; margin-left: 155px; font-family: Helvetica; color: steelblue"

{-  Does not provide functionality at the moment,
    but will be used to set up links and other
    nonessential UI elements.                      -}
setup :: MonadWidget t m => m ()
setup = el "div" $ do
  text "hello"
  el "br" (return ())

-- Standard div template for a div element with static attributes, returns its
-- Name wrapped in an Event when the element is dragged.
divAttr :: MonadWidget t m => (Map String String) -> String -> m (R.Event t String)
divAttr attrs name = do
  (e, _) <- elAttr' "div" attrs (text name)
  return $ fmap (const (name)) (domEvent Drag e)

-- Div template for a container which reacts to various events and returns the
-- EventID wrapped in an event.
{- *** Still doesn't work, not sure if the commented out lines have something to do with it
       But it won't compile with them uncommented, getting it to compile may be the answer  *** -}
divDynAttr :: forall t m. R.MonadWidget t m => R.Dynamic t (Map String String) -> R.Dynamic t String -> m (R.Event t DragEvent)
divDynAttr dynAttrs name = do
  (e, child) <- elDynAttr' "container" dynAttrs $ dynText name
  x <- R.wrapDomEvent (R._el_element e) GHCJS.elementOndrop stopAll
  y <- R.wrapDomEvent (R._el_element e) GHCJS.elementOndragend stopAll
  --_ <- R.performEvent_ x
  --_ <- R.performEvent_ y
  return $ R.leftmost [
    DragEnter <$ R.domEvent R.Dragover e,
    DragExit <$ R.domEvent R.Dragend e,
    DragExit <$ R.domEvent R.Dragleave e,
    DragDrop <$ x,
    DragDrop <$ y
    ]

-- Builds a block which corresponds to a Tidal Sample (i.e sn bd bp...)
sampleBlock :: MonadWidget t m => String -> m (Dynamic t String)
sampleBlock name = do

  -- Create a divAttr template and pass the event
  b <- divAttr attrs name

  -- Store the blocks name when the element is dragged, else store ""
  blockname <- holdDyn "" b

  -- Testing
  dynText $ blockname

  -- Return the resulting block name
  return $ blockname

  -- Set the elements attributes
  where
    attrs = Data.Map.fromList
            [("draggable", "true"),
             ("class", "sampleBlock"),
             ("style", "fontsize: 10px;" ++
              "font-family: Helvetica;" ++
              "background-color: steelblue")]

-- Create a section listing all the available samples
sampleWidget :: MonadWidget t m => m (Dynamic t String)
sampleWidget = elClass "div" "sampleWidget" $ do

  -- Find out which element is currently being dragged and return its name
  blockName <- (sampleBlock "bd")
  return $ blockName

-- Tuple template
tuple x y = (x,y)

-- Create a widget container that will react to dragOver, dragLeave, and Drop events.
sampleWidgetContainer :: (MonadWidget t m) => Dynamic t String -> m ()
sampleWidgetContainer blockName = do
  -- Recursively build the dynamic div
  rec b     <- divDynAttr attrs name

      -- Get the eventID i.e which event fired
      dragEvent <- holdDyn Empty b

      -- Pass eventID to the whichAttr pure function in order to determine
      -- which attributes to display
      attrs <- forDyn (dragEvent) whichAttr

      -- Create a tuple containing the eventID and block name
      dis <- combineDyn (tuple) dragEvent blockName

      -- If there is a drop event, change the containers name to that of the
      -- dropped element.
      name <- forDyn dis (\(i,s) ->
        (if i == DragDrop
          then s
          else ""))

      -- Testing
      dynText $ name
  (return ())

-- Set a dynamic div's attributes based on the type of event that fired
whichAttr :: DragEvent -> (Map String String)
whichAttr conState
         | conState == DragEnter = Data.Map.fromList
                               [("style", "border: 3px dotted white; " ++
                                 "position: absolute; left: 100px; top: 300px; width: 200px; height: 200px;")]
         | conState == DragExit = Data.Map.fromList
                               [("style", "border: 1px solid purple; " ++
                                 "position: absolute; left: 100px; top: 300px; width: 200px; height: 200px;")]
         | conState == DragDrop = Data.Map.fromList
                               [("style", "border: 2px solid purple; " ++
                                 "position: absolute; left: 100px; top: 300px; width: 200px; height: 200px;")]
         | otherwise     = Data.Map.fromList
                               [("style", "border: 1px solid purple; " ++
                                 "position: absolute; left: 100px; top: 300px; width: 200px; height: 200px;")]
