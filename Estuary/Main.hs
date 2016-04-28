-- Matthew Paine
-- January 28 2016
-- Estuary UI : Reflex/GHCJS front end for Tidal
-- My attempt at creating a drag and drop interface with Reflex and GHCJS
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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
import           Data.Map (Map, fromList, maxView, insert, fold, adjust, delete, findMax, elemAt, partitionWithKey, mapKeys, union)
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

initialState :: Map Int Info
initialState = Data.Map.fromList [(0,info)]
 where info = ("bd",(1,1),Empty)

appendToState :: Info -> Map Int Info -> Map Int Info
appendToState info xs = Data.Map.insert ((+1) $ fst $ Data.Map.findMax xs) info xs

removeFromState :: Int -> Map Int Info -> Map Int Info
removeFromState key xs = Data.Map.delete key xs

getPattFromState :: Map Int Info -> String
getPattFromState xs = Data.Map.fold (\(p,(s,n),b) e ->
  (if      n>1 && s>1  then (p ++ ":" ++ show(s) ++ "*" ++ show(n) ++ " " ++ e)
   else if n>1 && s==1 then (p ++ "*" ++ show(n) ++ " " ++ e)
   else if n==1 && s>1 then (p ++ ":" ++ show(s) ++ " " ++ e)
   else (p ++ " " ++ e)
  )) "" xs

updateState :: (Int,Info) -> Map Int Info -> Map Int Info
updateState tuple xs = Data.Map.adjust (\_ -> snd tuple) (fst tuple) xs

-- Insert the element before/after the dropped element.
insertInState :: ((Int,Info),(Int, Info)) -> Map Int Info -> Map Int Info
insertInState ((pk,pinfo),(ck,cinfo)) xs
  | (((\(a,b,c) -> c) pinfo) == DragE) && ( ((\(a,b,c) -> c) cinfo) == DragendE) = do
          let nxs = Data.Map.delete pk xs
          let tupleMap = Data.Map.partitionWithKey (\ k _ -> k > ck) nxs
          let hMap = Data.Map.mapKeys (+1) (fst tupleMap)
          let lMap = Data.Map.insert ((+1) $ fst $ Data.Map.findMax $ snd tupleMap) (pinfo) (snd tupleMap)
          Data.Map.union lMap hMap
  | otherwise = do
          xs

    -- Need to assign an appropriate key. blockfk > insert > blocklk
    -- Partition map at blockfk                                       : partitionWithKey (\ k _ -> k > blockfk)
    -- Increase key values of partition by 1                          : mapKeys (+1)
    -- Insert new element at end of map                               : insert
    -- Append the partition                                           : union

blockAppender :: R.MonadWidget t m => m (R.Event t (Map Int Info -> Map Int Info))
blockAppender = do
  paletteE <- palette
  return $ fmap (appendToState) paletteE

blockRemover :: R.MonadWidget t m => Dynamic t Int -> m (R.Event t (Map Int Info -> Map Int Info))
blockRemover keyD = do
  removerE <- removerWidget
  let removeE = tag (current keyD) removerE
  return $ fmap (removeFromState) removeE

blockUpdater :: R.MonadWidget t m => Dynamic t (Int,Info) -> m (R.Event t (Map Int Info -> Map Int Info))
blockUpdater blockDTuple = do
  updaterE <- updaterWidget
  let updateE = tag (current blockDTuple) updaterE
  return $ fmap (updateState) updateE

-- TO DO (Event t (Int,Int) :: (Drop Event (key of block dropped on, key of block dropped)))
blockInserter :: R.MonadWidget t m => Dynamic t ((Int,Info),(Int,Info)) -> m (R.Event t (Map Int Info -> Map Int Info))
blockInserter blocksDyn = do
  inserterE <- updaterWidget
  --let inserterE = (updated blocksDyn)
  let insertE = tag (current blocksDyn) inserterE
  return $ fmap (insertInState) insertE

blockWidget :: R.MonadWidget t m => m ()
blockWidget = mdo
  -- Event t (Key :: Int, Info :: Info (Sample :: String, (Sample Number :: Int, Multiplier :: Int), Key :: Int, Box Event :: BoxEvent)))
  blockTupleE <- createContainerEl keyD dynamicMap
  -- Dynamic t (Key :: Int, Info :: Info (Sample :: String, (Sample Number :: Int, Multiplier :: Int), Key :: Int, Box Event :: BoxEvent)))
  blockDTuple <- holdDyn (0,("",(1,1),Empty)) blockTupleE
  -- Event t (Old Value, New Value)
  let blockEvents = attach (current blockDTuple) (updated blockDTuple)
  blocksDyn <- holdDyn ((0,("",(1,1),Empty)),(0,("",(1,1),Empty))) blockEvents
  -- (Key :: Dynamic t Int, Info :: Dynamic t (Sample :: String, (Sample Number :: Int, Multiplier :: Int), Key :: Int, Box Event :: BoxEvent))
  blockTupleD <- splitDyn blockDTuple
  -- Dynamic t Int
  let keyD = fst blockTupleD
  -- Dynamic t (Sample :: String, (Sample Number :: Int, Multiplier :: Int), Key :: Int, Box Event :: BoxEvent)
  let dynInfo = snd blockTupleD

  events <- sequence [blockRemover keyD, blockAppender, blockUpdater blockDTuple, blockInserter blocksDyn]
  dynamicMap <- foldDyn ($) initialState (leftmost events)
  -----------------Testing------------------
  --patt <- forDyn dynamicMap getPattFromState
  --display patt
  --showarc <- forDyn patt showAllArcs
  --display showarc
  --arcs <- forDyn patt extractArcs
  --display arcs
  ------------------------------------------
  return ()

removerWidget :: MonadWidget t m => m (R.Event t ())
removerWidget = do
  -- switch to delete key
  removeE <- button "Remove Block"
  return $ removeE

updaterWidget :: MonadWidget t m => m (R.Event t ())
updaterWidget = do
  -- switch to command enter
  updateE <- button "Update Pattern"
  return $ updateE

createContainerEl :: MonadWidget t m => Dynamic t Int -> Dynamic t (Map Int Info) -> m (R.Event t (Int, Info))
createContainerEl dynKey dynamicMap = mdo
  infoE <- elAttr "div" conAttrs $ do
    info <- selectViewListWithKey dynKey dynamicMap displaySampleBlock
    return $ info
  return $ infoE
  where conAttrs = Data.Map.fromList [("draggable", "true"),("style", "position: relative; height: 500px;" ++
                                       "border: 1px solid black; background-color: light-blue" ++
                                       "display: block;")]

createBoxEl :: MonadWidget t m => Dynamic t Info -> Dynamic t (Map String String) -> m (El t)
createBoxEl dynInfo attrsDyn = do
  (boxEl, _) <- elDynAttr' "div" attrsDyn $ do
    name <- forDyn dynInfo (\(a,b,c) -> a)
    display $ name
  return $ boxEl

displaySampleBlock :: MonadWidget t m => Int -> Dynamic t Info -> Dynamic t Bool -> m (R.Event t Info)
displaySampleBlock key dynInfo isSelected = mdo
  boxEl <- createBoxEl dynInfo attrsDyn

  --let test = constDyn key
  --display test
  --test <- forDyn isSelected (\x -> if x == True then "true" else "false")
  --display test

  mousePosE <- wrapDomEvent (R._el_element boxEl) (R.onEventName R.Drag) getMouseEventCoords
  --test <- wrapDomEvent (R._el_element boxEl) (R.onEventName R.Drop) (void $ GHCJS.preventDefault)
  x <- R.wrapDomEvent (R._el_element boxEl) (R.onEventName R.Drop) (void $ GHCJS.preventDefault)
  y <- R.wrapDomEvent (R._el_element boxEl) (R.onEventName R.Dragover) (void $ GHCJS.preventDefault)
  z <- R.wrapDomEvent (R._el_element boxEl) (R.onEventName R.Dragend) (void $ GHCJS.preventDefault)
  pos <- holdDyn (0,0) mousePosE
  --_ <- R.performEvent_ $ return () <$ x
  _ <- R.performEvent_ $ return () <$ y
  --_ <- R.performEvent_ $ return () <$ z
  --display pos

  -----------------------------------------------------------------
  -- spacebar -> increment sample
  -- up/down arrow -> inc/dec multiplier
  -- If block gets clicked
  -- increment counter change name (return in info)
  --nClicks <- foldDyn (\() -> succ) (1 :: Int) (_el_clicked boxEl)
  --tuple <- combineDyn (\a b -> (a,b)) nClicks dynInfo
  --ndynInfo <- forDyn tuple (\(x,(p,s,n)) -> d ++ ":" ++ show(n))
  -----------------------------------------------------------------

  boxDomE <- return $ leftmost [ClickE <$ R.domEvent R.Click boxEl,
                                DragE  <$ R.domEvent R.Drag boxEl,
                                --DragoverE <$ R.domEvent R.Dragover boxEl,
                                DragendE <$ x, DropE <$ x]

  boxDyn <- holdDyn Empty boxDomE
  selectEvent <- return $ updated isSelected
  selectE <- return $ attachDyn boxDyn selectEvent
  boxEvent <- return $ attachDyn pos selectE
  boxD <- holdDyn ((0,0),(Empty,False)) boxEvent

  attrsDyn <- forDyn boxD determineBoxAttributes

  -- Switch BoxEvent in dynInfo
  tuple <- combineDyn (\a b -> (a,b)) dynInfo boxDyn
  ndynInfo <- forDyn tuple (\((p,(s,n),b),be) -> (p,(s,n),be))
  --display ndynInfo
  -- return new boxEvent

  boxE <- return $ tagDyn ndynInfo boxDomE

  return $ boxE

determineBoxAttributes :: ((Int,Int),(BoxEvent,Bool)) -> Map String String
determineBoxAttributes ((x,y),(boxEvent,selected))
        | boxEvent == ClickE && selected = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width:30px; background-color: hsl(80,80%,30%);" ++
              "height: 30px; float: left; border: 3px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em;")] --left:" ++ show (x) ++ "px; top:" ++ show(y) ++ "px;")]
        | boxEvent == DragE && selected  = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width:30px; background-color: hsl(80,80%,50%);" ++
              "height: 30px; float: left; border: 1px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em; left:")]-- ++ show (x) ++ "px; top:" ++ show(y) ++ "px;")]
        | boxEvent == DropE && selected  = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width: 30px; background-color: hsl(80,80%,50%);" ++
              "height: 30px; float: left; border: 1px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em; left:")]-- ++ show (x) ++ "px; top:" ++ show(y) ++ "px;")]
        | boxEvent == DragoverE && selected  = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width:30px; background-color: hsl(80,80%,30%);" ++
              "height: 30px; float: left; border: 1px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em; left:")]-- ++ show (x) ++ "px; top:" ++ show(y) ++ "px;")]
        | boxEvent == HoveroverE && selected  = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width:30px; background-color: hsl(80,80%,30%);" ++
              "height: 30px; float: left; border: 1px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em; left:")]-- ++ show (x) ++ "px; top:" ++ show(y) ++ "px;")]
        | otherwise            = Data.Map.fromList
            [("draggable", "true"),("class","countBin noselect")
            ,("style","width: 30px; background-color: hsl(80,80%,50%);" ++
              "height: 30px; float: left; border: 1px solid black; position: relative;" ++
              "display:block; padding:.3em 0.5em; left:")]-- ++ show (x) ++ "px; top:" ++ show(y) ++ "px;")]

paletteEl :: MonadWidget t m => (Map String String) -> String -> m (R.Event t Info)
paletteEl attrs name = do
  (e,_) <- elAttr' "li" attrs $ text name
  return $ ((name,(1,1),Empty) <$ R.domEvent R.Click e)

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

-- Drag and drop
-- Need the dynInfo of the dropped element.
    -- pass in the key of the previous element to the current element
    -- not working through the key? Try through dynInfo...
    -- can't pass through dynInfo, have to test how dragover and drop events effect isSelected
-- Need the key of the element dropped on.
    -- same problem
-- Insert the element before/after the dropped element.
    -- Need to assign an appropriate key. blockfk > insert > blocklk
    -- Partition map at blockfk                                       : partitionWithKey (\ k _ -> k > blockfk)
    -- Increase key values of partition by 1                          : mapKeys (+1)
    -- Insert new element at end of map                               : insert
    -- Append the partition                                           : union
