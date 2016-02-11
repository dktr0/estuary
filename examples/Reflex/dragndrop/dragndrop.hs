-- Matthew Paine
-- January 28 2016
-- Estuary UI : Reflex/GHCJS front end for Tidal
-- My attempt at creating a drag and drop interface with Reflex and GHCJS
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ExistentialQuantification #-}

-- Haskell Imports
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Tuple (fst, snd)
import           Data.String
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

data DragEvent = DragEnter | DragExit | DragDrop | DragDrag | DragEnd | DragClick | Empty
  deriving (Eq, Show)

-- Tuple template
tuple :: a -> b -> (a,b)
tuple x y = (x,y)

-- Triple template
triple :: (a,b) -> c -> (a,b,c)
triple (x,y) z = (x,y,z)

fst' :: (a,b,c) -> a
fst' (x,_,_) = x

snd' :: (a,b,c) -> b
snd' (_,y,_) = y

thd' :: (a,b,c) -> c
thd' (_,_,z) = z

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
divAttr :: MonadWidget t m => (Map String String) -> String -> m (R.Event t (DragEvent,String) )
divAttr attrs name = do
  (e, _) <- elAttr' "button" attrs (text name)
  x <- R.wrapDomEvent(R._el_element e) GHCJS.elementOndragend stopAll
  return $ R.leftmost [ (DragDrag,name) <$ domEvent Drag e , (DragEnd,name) <$ x ]

-- Div template for a container which reacts to various events and returns the
-- EventID wrapped in an event
divDynAttr :: forall t m. R.MonadWidget t m => R.Dynamic t (Map String String) -> R.Dynamic t String -> m (R.Event t DragEvent)
divDynAttr dynAttrs name = do
  (e, _) <- elDynAttr' "div" dynAttrs $ dynText name
  x <- R.wrapDomEvent (R._el_element e) GHCJS.elementOndragover stopAll
  y <- R.wrapDomEvent (R._el_element e) GHCJS.elementOndrop stopAll
  z <- R.wrapDomEvent (R._el_element e) GHCJS.elementOndragend stopAll
  --_ <- R.performEvent_ x
  --_ <- R.performEvent_ y
  return $ R.leftmost [
    DragEnter <$ x,
    DragExit <$ R.domEvent R.Dragend e,
    DragExit <$ R.domEvent R.Dragleave e,
    DragDrag <$ R.domEvent R.Drag e,
    DragClick <$ R.domEvent R.Click e,
    DragDrop <$ y,
    DragEnd <$ z
    ]

-- Builds a block which corresponds to a Tidal Sample (i.e sn bd bp...)
sampleBlock :: MonadWidget t m => String -> m (Dynamic t String)
sampleBlock name = do

  -- Create a divAttr template and pass the event
  b <- divAttr attrs name

  -- Store the blocks name when the element is dragged, else store ""
  dynTuple <- holdDyn (Empty,"") b

  -- Extract the elements out of the tuple
  blockname <- forDyn dynTuple snd
  -- event <- forDyn dynTuple fst

  {-
  blockname <- forDyn dynTuple (\(i,s) ->
    (if i == DragDrag
      then s
      else ""))
  -}

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
  bd <- sampleBlock "bd"
  sn <- sampleBlock "sn"
  bp <- sampleBlock "bp"
  arpy <- sampleBlock "arpy"

  --blockName <- sampleBlock "bp"
  let n = [bd,sn,bp,arpy]

  blockName <- mconcatDyn n

  --dynText blockName
  return $ blockName

{-
createSampleBlock :: MonadWidget t m => Dynamic t String -> Dynamic t Int -> m (Dynamic t String)
createSampleBlock blockname num = do
  rec b     <- divDynAttr attrs name
      let name = blockname
      let s = constDyn ("Drop Here!"::String)
      attrs <- forDyn num sampleBlockAttrs
      return $ s
      -- onclick behaviour
      -- ondrop behaviour

sampleBlockAttrs :: Int -> (Map String String)
sampleBlockAttrs num = Data.Map.fromList [("style","position: absolute; left" ++ show (20*(num-1)) ++ "px;" ++
                                      "width: 20px; height: 20px;")]
-}

-- Create a widget container that will react to dragOver, dragLeave, and Drop events.
sampleWidgetContainer :: (MonadWidget t m) => Dynamic t String -> m (Dynamic t Int)
sampleWidgetContainer blockName = do
  -- Recursively build the dynamic div
  rec b     <- divDynAttr attrs name

      let oldname = name

      dragEvent <- holdDyn Empty b
      incrementer <- count b

      attrs <- forDyn (dragEvent) whichAttr

      dis <- combineDyn (tuple) dragEvent blockName
      --tis <- combineDyn (triple) dis blockName

      -- Set container name idea
      --dis <- combineDyn (tuple) dragEvent blockName
      --tis <- combineDyn (triple) dis oldname
      --thing <- forDyn tis setContainerName
      --let name = thing

      name <- forDyn dis (\(i,s) ->
        (if i == DragDrop
          then s
          else ""))

  (return incrementer)

{-
setContainerName :: (DragEvent,String,String) -> String
setContainerName triple
         | (( (fst' triple) == DragDrop) && ((snd' triple) > "") ) = snd' triple
         | otherwise = thd' triple
-}

-- Set a dynamic div's attributes based on the type of event that fired
whichAttr :: DragEvent -> (Map String String)
whichAttr conState
         | conState == DragEnter = Data.Map.fromList
                               [("style", "border: 3px dotted white;" ++
                                 "position: absolute; left: 100px; top: 300px; width: 200px; height: 200px;")]
         | conState == DragExit = Data.Map.fromList
                               [("style", "border: 1px solid purple;" ++
                                 "position: absolute; left: 100px; top: 300px; width: 200px; height: 200px;")]
         | conState == DragDrop = Data.Map.fromList
                               [("style", "border: 2px solid purple;" ++
                                 "position: absolute; left: 100px; top: 300px; width: 200px; height: 200px;")]
         | otherwise     = Data.Map.fromList
                               [("style", "border: 1px solid purple;" ++
                                 "position: absolute; left: 100px; top: 300px; width: 200px; height: 200px;")]
