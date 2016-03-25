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
import           Data.Map (Map, fromList, maxView, insert, fold, adjust)
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
import           GHCJS.DOM.MouseEvent as GHCJS
import qualified GHCJS.DOM.Element as GHCJS
import qualified GHCJS.DOM.EventM as GHCJS

main :: IO ()
main = mainWidget $ do
  elAttr "div" ("style" =: s) $ text "Estuary"
  blockWidget
  where
    s = "font-size: 50px; margin-left: 155px; font-family: Helvetica; color: steelblue"

-- Implements the drag and drop functionality for the sample blocks.
-- for now Info is just the name of the sample but it will be much more involved later on
type Info = String

initialState :: Map Int Info
initialState = fromList [(0,info)]
 where info = "bd"

appendToState :: Info -> Map Int Info -> Map Int Info
appendToState info xs = Data.Map.insert (length xs) info xs

removeFromState :: Int -> Map Int Info -> Map Int Info
removeFromState key xs = case Data.Map.maxView xs of
  Nothing -> xs
  Just (_,xs') -> xs'

getPattFromState :: Map Int Info -> String
getPattFromState xs = Data.Map.fold (++) "" xs

updateState :: (Int,Info) -> Map Int Info -> Map Int Info
updateState tuple xs = Data.Map.adjust ( (snd tuple) ++) (fst tuple) xs

blockAppender :: R.MonadWidget t m => m (R.Event t (Map Int Info -> Map Int Info))
blockAppender = do
  paletteE <- palette
  return $ fmap (appendToState) paletteE

blockRemover :: R.MonadWidget t m => Dynamic t Int -> m (R.Event t (Map Int Info -> Map Int Info))
blockRemover keyD = do
  removerE <- removerWidget
  removeE <- return $ tagDyn keyD removerE
  return $ fmap (removeFromState) removeE

blockUpdater :: R.MonadWidget t m => R.Event t (Int,Info) -> m (R.Event t (Map Int Info -> Map Int Info))
blockUpdater updateE = do
  return $ fmap (updateState) updateE

-------------------------Need to add blockUpdater event--------------------------
blockWidget :: R.MonadWidget t m => m ()
blockWidget = mdo

  blockTupleE <- createContainerEl dynamicMap keyD
  blockDTuple <- holdDyn (0,"") blockTupleE
  --updateE <- update dynamicMap

  blockTupleD <- splitDyn blockDTuple

  keyD <- return $ fst blockTupleD
  infoD <- snd blockTupleD

  events <- sequence [blockRemover keyD,blockAppender,blockUpdater blockTupleE]
  dynamicMap <- foldDyn ($) initialState (leftmost events)

  -----------------Testing------------------
  patt <- forDyn dynamicMap getPattFromState
  display patt
  showarc <- forDyn patt showAllArcs
  display showarc
  arcs <- forDyn patt extractArcs
  display arcs
  ------------------------------------------
  return (())

removerWidget :: MonadWidget t m => m (R.Event t ())
removerWidget = do
  removeE <- button "Remove Block"
  return $ removeE

createContainerEl :: MonadWidget t m => Dynamic t Int -> Dynamic t (Map Int Info) -> m (R.Event t (Int, Info))
createContainerEl dynKey dynamicMap = mdo
  (container, tupleE) <- elAttr' "div" conAttrs $ do
    tuple <- R.selectViewListWithKey dynKey dynamicMap displaySampleBlock
    return $ tuple
  return $ tupleE
  where conAttrs = Data.Map.fromList [("style", "position: relative; top: 50px; height: 500px;" ++
                                       "border: 1px solid black; background-color: light-blue" ++
                                       "display: block;")]

createBoxEl :: MonadWidget t m => Dynamic t Info -> Dynamic t (Map String String) -> m (El t)
createBoxEl dynInfo attrsDyn = do
  (boxEl, _) <- elDynAttr' "div" attrsDyn $ do
    display dynInfo
  return $ boxEl

displaySampleBlock :: MonadWidget t m => Int -> Dynamic t Info -> Dynamic t Bool -> m (R.Event t (Int, Info))
displaySampleBlock key dynInfo isSelected = do
  boxEl <- createBoxEl dynInfo

  -- If the block is currently selected (isSelected = true)
  -- some behaviour

  mousePosE <- wrapDomEvent (R._el_element boxEl) (R.onEventName R.Drag) getMouseEventCoords
  pos <- holdDyn (0,0) mousePosE
  display pos

  -- If block gets clicked
  -- increment counter change name (return in info)
  -- requires an update

  attrsDyn <- forDyn pos $ \(a,b) ->
    Data.Map.fromList
    [("draggable", "true"),("class","countBin noselect")
    ,("style","width:" ++ show (b*0 + 30) ++ "px;" ++
      "background-color: hsl("++ show (b* 0 + 10) ++ ",50%,50%);" ++
      "height: 30px; float: left; border: 1px solid black; position: relative;" ++
      "display:block; padding:.3em 0.5em; left:" ++ show (a) ++ "px; top:" ++ show(b) ++ "px;")]

  boxDomE <- leftmost [(key, dynInfo) <$ R.domEvent R.Click boxEl]

  boxE <- attachDyn dynInfo boxDomE

  -- return key of block if selected along with its info
  return $ boxE

paletteEl :: MonadWidget t m => (Map String String) -> String -> m (R.Event t Info)
paletteEl attrs name = do
  (e,_) <- elAttr' "li" attrs $ text name
  return $ ( name <$ R.domEvent R.Click e)

palette :: MonadWidget t m => m (R.Event t Info)
palette = do
    elAttr "ul" ulAttrs $ do
      bd <- paletteEl liAttrs " bd"
      sn <- paletteEl liAttrs " sn"
      arpy <- paletteEl liAttrs " arpy"
      arp <- paletteEl liAttrs " arp"
      hh <- paletteEl liAttrs " hh"
      ht <- paletteEl liAttrs " ht"
      cp <- paletteEl liAttrs " cp"
      (return $ R.leftmost [bd,sn,arpy,arp,hh,ht,cp])
    where
      liAttrs = Data.Map.fromList
              [ ("style", "fontsize: 6px;" ++
                "position: relative; float: left; text-decoration:none;" ++
                "display:block; padding:.8em 2.2em; background:#cde;" ++
                "border:1px solid #ccc; ")]
      ulAttrs = Data.Map.fromList
              [("style", "list-style: none; margin:0; padding:0; position: absolute")]
