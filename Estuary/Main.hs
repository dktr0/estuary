-- Matthew Paine
-- January 28 2016
-- Estuary UI : Reflex/GHCJS front end for Tidal
-- My attempt at creating a drag and drop interface with Reflex and GHCJS
{-# LANGUAGE RecursiveDo #-}

import           Sound.Tidal.Context as Tidal

import           Tidal.Utils

-- Haskell Imports
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Tuple (fst, snd)
import           Data.String
import           Data.Default
import           Data.Text (Text, intercalate)
import           Data.Array
import           Data.Maybe
import           Data.Map --(Map, fromList, maxView, insert, fold, adjust, delete, findMax, elemAt, partitionWithKey, mapKeys, union, empty)
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

data BoxEvent = ClickE | DragE | DropE | DragoverE | DragendE | HoveroverE | Empty
  deriving (Eq, Show)

-- Info :: (Sample, (Sample Number, Multiplier),BoxEvent)
type Info = (String,(Int,Int),BoxEvent)

-- The initial state of the map of block widgets
initialState :: Map Int Info
initialState = Data.Map.fromList [(0,info)]
 where info = ("bd",(1,1),Empty)

-- Append a new block to the map of blocks
-- Takes the info of the new block to be added along with the current map.
appendToState :: Info -> Map Int Info -> Map Int Info
appendToState info currentMap = Data.Map.insert ((+1) $ fst $ Data.Map.findMax currentMap) info currentMap

-- Remove a block from the map of blocks
-- Takes a map of the blocks to be removed (at this point 1) and the current map
removeFromState :: Map Int Info -> Map Int Info -> Map Int Info
removeFromState elementMap currentMap = Data.Map.difference currentMap elementMap

-- Concatonates the current tidal pattern out of the name info of each individual block
getPattFromState :: Map Int Info -> String
getPattFromState currentMap = Data.Map.fold (\(p,(s,n),b) e ->
  (if      n>1 && s>1  then (p ++ ":" ++ show(s) ++ "*" ++ show(n) ++ " " ++ e)
   else if n>1 && s==1 then (p ++ "*" ++ show(n) ++ " " ++ e)
   else if n==1 && s>1 then (p ++ ":" ++ show(s) ++ " " ++ e)
   else (p ++ " " ++ e)
  )) "" currentMap

-- Updates the block map based on any changes or additions to the newMap
-- Takes in the newly changes version of the dyamic block map and the current map
updateState :: Map Int Info -> Map Int Info -> Map Int Info
updateState newMap currentMap = Data.Map.union currentMap newMap

-- Insert the currently dragged block element into a position in the map.
-- Takes the old map of elements, the map of the element to be inserted, and the current map
insertInState :: ((Map Int Info),(Map Int Info)) -> Map Int Info -> Map Int Info
insertInState (oldMap,elementMap) currentMap
  |( ( ( (\(a,b,c) -> c) (snd $ head $ Data.Map.assocs oldMap) ) == DragE ) && ( ((\(a,b,c) -> c) (snd $ head $ Data.Map.assocs newMap)) == DragendE) ) = do
          let oldList = Data.Map.assocs oldMap
          let currentList = Data.Map.assocs elementMap
          let newMap = Data.Map.delete (fst $ head $ pList) currentMap
          let tupleMap = Data.Map.partitionWithKey (\ k _ -> k > (fst $ head $ cList)) newMap
          let highMap = Data.Map.mapKeys (+1) (fst tupleMap)
          let lowMap = Data.Map.insert ((+1) $ fst $ Data.Map.findMax $ snd tupleMap) (snd $ head $ oldList) (snd tupleMap)
          Data.Map.union lowMap highMap
  | otherwise = do
          currentMap

-- Calls the appendToState function on a palette click event returns the function call wrapped in the event
blockAppender :: R.MonadWidget t m => m (R.Event t (Map Int Info -> Map Int Info))
blockAppender = do
  paletteE <- palette
  return $ fmap (appendToState) paletteE

-- Removes the selected block on click of the remover Widget
-- Takes a the dynamic map of block elements and returns the removeFromState function call wrapped in the removerWidget click event
blockRemover :: R.MonadWidget t m => Dynamic t (Map Int Info) -> m (R.Event t (Map Int Info -> Map Int Info))
blockRemover dynamicBlockMap = do
  removerE <- removerWidget
  let removeE = tag (current dynamicBlockMap) removerE
  return $ fmap (removeFromState) removeE

-- Calls the updateState function on any fired block event
-- Needs to be fixed do to recursive issues
blockUpdater :: R.MonadWidget t m => R.Event t (Map Int Info) -> m (R.Event t (Map Int Info -> Map Int Info))
blockUpdater eventBlockMap = do
  behavior <- hold eventBlockMap
  return $ fmap (updateState) eventBlockMap

-- Calls insertInState function on any fired block event
-- Needs to be fixed do to recursive issues
blockInserter :: R.MonadWidget t m => R.Event t (Map Int Info,Map Int Info) -> m (R.Event t (Map Int Info -> Map Int Info))
blockInserter eventBlockMaps = do
  behavior <- hold eventBlockMaps
  return $ fmap (insertInState) eventBlockMaps

-- Main function which creates the container and block elements
-- Keeps track of the dynamic Map of blocks as it is mutated by events
blockWidget :: R.MonadWidget t m => m ()
blockWidget = mdo
  -- Behavior t (Map k (Event t Info))
  blockBehaviorMapEvent <- createContainerEl dynamicMap
  -- Behavior t (Event t (Map k Info))
  let blockEventMap = fmap (R.mergeMap) blockBehaviorMapEvent
  -- Event t (Map k Info)
  let eventBlockMap = R.switch blockEventMap
  -- Dynamic t (Map k Info)
  dynamicBlockMap <- holdDyn Data.Map.empty eventBlockMap
  -- Event t (Old :: (Map k Info), New :: (Map k Info))
  let eventBlockMaps = attach (current dynamicBlockMap) (updated dynamicBlockMap)
  -- Dynamic t (Map k Info, Map k Info)
  dynamicBlockMaps <- holdDyn (Data.Map.empty,Data.Map.empty) eventBlockMaps
  -- Commented out do to error which needs to be fixed
  blockEvents <- sequence [blockRemover dynamicBlockMap, blockAppender]--, blockUpdater eventBlockMap, blockInserter eventBlockMaps]
  --
  dynamicMap <- foldDyn ($) initialState (leftmost blockEvents)
  return ()

-- Create the container element and the block widgets which are to be placed inside of it
-- returns the current value of the dynamicMap along with whatever displaySampleBlock returns
createContainerEl :: MonadWidget t m => Dynamic t (Map Int Info) -> m (R.Behavior t (Map Int (R.Event t Info)))
createContainerEl dynamicMap = mdo
  infoE <- elAttr "div" conAttrs $ do
    info <- listViewWithKey' dynamicMap displaySampleBlock
    return $ info
  return $ infoE
  where conAttrs = Data.Map.fromList [("draggable", "true"),("style", "position: relative; height: 500px;" ++
                                       "border: 1px solid black; background-color: light-blue" ++
                                       "display: block;")]

-- Implements all functionality of an individual sample block
-- Returns its information wrapped in an event (Click, Drag, Drop)
displaySampleBlock :: MonadWidget t m => Int -> Dynamic t Info -> m (R.Event t Info)
displaySampleBlock key dynInfo = mdo
  boxEl <- createBoxEl dynInfo attrsDyn

  -- Event Listeners for drag and drop
  mousePosE <- wrapDomEvent (R._el_element boxEl) (R.onEventName R.Drag) getMouseEventCoords
  x <- R.wrapDomEvent (R._el_element boxEl) (R.onEventName R.Drop) (void $ GHCJS.preventDefault)
  y <- R.wrapDomEvent (R._el_element boxEl) (R.onEventName R.Dragover) (void $ GHCJS.preventDefault)
  z <- R.wrapDomEvent (R._el_element boxEl) (R.onEventName R.Dragend) (void $ GHCJS.preventDefault)
  pos <- holdDyn (0,0) mousePosE
  -- Performs event as necessary to implement dropping
  _ <- R.performEvent_ $ return () <$ y

  -- Event Feedback
  boxDomE <- return $ leftmost [ClickE <$ R.domEvent R.Click boxEl,
                                DragE  <$ R.domEvent R.Drag boxEl,
                                --DragoverE <$ R.domEvent R.Dragover boxEl,
                                DragendE <$ x, DropE <$ x]
  boxDyn <- holdDyn Empty boxDomE
  tuple <- combineDyn (\a b -> (a,b)) dynInfo boxDyn
  ndynInfo <- forDyn tuple (\((p,(s,n),b),be) -> (p,(s,n),be))
  boxE <- return $ tagDyn ndynInfo boxDomE

  -- Visual Feedback Stuff
  attrsDyn <- forDyn boxDyn determineBoxAttributes
  -----------------------------------------------------------------
  -- spacebar -> increment sample
  -- up/down arrow -> inc/dec multiplier
  -- If block gets clicked
  -- increment counter change name (return in info)
  --nClicks <- foldDyn (\() -> succ) (1 :: Int) (_el_clicked boxEl)
  --tuple <- combineDyn (\a b -> (a,b)) nClicks dynInfo
  --ndynInfo <- forDyn tuple (\(x,(p,s,n)) -> d ++ ":" ++ show(n))
  -----------------------------------------------------------------
  return $ boxE

-- Creates an individual block element
-- Takes the block info and a map of dynamic attributes, returns the element
createBoxEl :: MonadWidget t m => Dynamic t Info -> Dynamic t (Map String String) -> m (El t)
createBoxEl dynInfo attrsDyn = do
  (boxEl, _) <- elDynAttr' "div" attrsDyn $ do
    name <- forDyn dynInfo (\(a,b,c) -> a)
    display $ name
  return $ boxEl

-- Determines a blocks attribute based on which event has fired
-- Need to fix this as attributes will stay the same until a different event has fired on the blocks
-- Need to listen for an event to revert all block's boxEvent's to empty
determineBoxAttributes :: BoxEvent -> Map String String
determineBoxAttributes boxEvent
        | boxEvent == ClickE = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width:30px; background-color: hsl(80,80%,30%);" ++
              "height: 30px; float: left; border: 3px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em;")] --left:" ++ show (x) ++ "px; top:" ++ show(y) ++ "px;")]
        | boxEvent == DragE = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width:30px; background-color: hsl(80,80%,50%);" ++
              "height: 30px; float: left; border: 1px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em; left:")]-- ++ show (x) ++ "px; top:" ++ show(y) ++ "px;")]
        | boxEvent == DropE = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width: 30px; background-color: hsl(80,80%,50%);" ++
              "height: 30px; float: left; border: 1px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em; left:")]-- ++ show (x) ++ "px; top:" ++ show(y) ++ "px;")]
        | boxEvent == DragoverE = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width:30px; background-color: hsl(80,80%,30%);" ++
              "height: 30px; float: left; border: 1px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em; left:")]-- ++ show (x) ++ "px; top:" ++ show(y) ++ "px;")]
        | boxEvent == HoveroverE = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width:30px; background-color: hsl(80,80%,30%);" ++
              "height: 30px; float: left; border: 1px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em; left:")]-- ++ show (x) ++ "px; top:" ++ show(y) ++ "px;")]
        | otherwise            = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width: 30px; background-color: hsl(80,80%,50%);" ++
              "height: 30px; float: left; border: 1px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em; left:")]-- ++ show (x) ++ "px; top:" ++ show(y) ++ "px;")]

-- Implements the remove button and returns its event
removerWidget :: MonadWidget t m => m (R.Event t ())
removerWidget = do
  removeE <- button "Remove Block"
  return $ removeE

-- Implements the update button and returns its event
updaterWidget :: MonadWidget t m => m (R.Event t ())
updaterWidget = do
  updateE <- button "Update Pattern"
  return $ updateE

-- Creates an individual palette alement and listens for a click event
-- If the element is clicked it returns default block info with the corresponding sample name to be assigned to a newly created block
paletteEl :: MonadWidget t m => (Map String String) -> String -> m (R.Event t Info)
paletteEl attrs name = do
  (e,_) <- elAttr' "li" attrs $ text name
  return $ ((name,(1,1),Empty) <$ R.domEvent R.Click e)

-- Creates the palette of different samples to be clicked
-- Returns the info of the element to be added to the map on the event returns from paletteEl
palette :: MonadWidget t m => m (R.Event t Info)
palette = do
    elAttr "ul" ulAttrs $ do
      bd <- paletteEl liAttrs "bd"
      sn <- paletteEl liAttrs "sn"
      arpy <- paletteEl liAttrs "arpy"
      arp <- paletteEl liAttrs "arp"
      hh <- paletteEl liAttrs "hh"
      ht <- paletteEl liAttrs "ht"
      cp <- paletteEl liAttrs "cp"
      (return $ R.leftmost [bd,sn,arpy,arp,hh,ht,cp])
    where
      liAttrs = Data.Map.fromList
              [ ("style", "fontsize: 6px;" ++
                "position: relative; float: left; text-decoration:none;" ++
                "display:block; padding:.8em 2.2em; background:#cde;" ++
                "border:1px solid #ccc; ")]
      ulAttrs = Data.Map.fromList
              [("style", "list-style: none; margin:0; padding:0; position: absolute")]

    {-
      [17:22] <ryantrinkle> you could have each one return an Event t ()
      [17:22] <ryantrinkle> then, your list widget would collect those into Event t (Map k (Event t Info))
      [17:22] <ryantrinkle> you can hold that (using Map.empty as the initial value)
      [17:23] <ryantrinkle> which will give you Behavior t (Map k (Event t Info))
      [17:23] <ryantrinkle> then, you can fmap mergeMap over that
      [17:23] <ryantrinkle> giving you: Behavior t (Event t (Map k Info))
      [17:23] <ryantrinkle> then switch
      [17:23] <ryantrinkle> giving: Event t (Map k Info)
      [17:24] <ryantrinkle> the keys of that map will be the items that want to be deleted :)
    -}
