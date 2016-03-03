-- Matthew Paine
-- January 28 2016
-- Estuary UI : Reflex/GHCJS front end for Tidal
-- My attempt at creating a drag and drop interface with Reflex and GHCJS
{-# LANGUAGE RecursiveDo #-}

--import           Sound.Tidal.Context as Tidal

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
import           GHCJS.DOM.MouseEvent as GHCJS
import qualified GHCJS.DOM.Element as GHCJS
import qualified GHCJS.DOM.EventM as GHCJS

-- Function that implements the prevent default java action so that drop events can be detected.
{-
stopAll :: GHCJS.IsEvent event => GHCJS.EventM event e ()
stopAll = do
  GHCJS.preventDefault
  GHCJS.stopPropagation
  return()
-}

main :: IO ()
main = mainWidget $ do
  elAttr "div" ("style" =: s) $ text "Estuary"
  sPatternContainer
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

removeFromState :: Map Int Info -> Map Int Info
removeFromState xs = case Data.Map.maxView xs of
  Nothing -> xs
  Just (_,xs') -> xs'

blockAppender :: R.MonadWidget t m => m (R.Event t (Map Int Info -> Map Int Info))
blockAppender = do
  paletteE <- palette
  return $ fmap (appendToState) paletteE

blockRemover :: R.MonadWidget t m => m (R.Event t (Map Int Info -> Map Int Info))
blockRemover = do
  removeE <- button "Remove Sample"
  return $ fmap (\() -> removeFromState) removeE

sPatternContainer :: R.MonadWidget t m => m ()
sPatternContainer = do
  events <- sequence [blockRemover,blockAppender]
  dynamicMap <- foldDyn ($) initialState (leftmost events)
  (container, _) <- elAttr' "div" conAttrs $ do
    listWithKey dynamicMap displaySampleBlock
    el "br" (return ())
  return (())
  where conAttrs = Data.Map.fromList [("style", "position: relative; top: 25px; height: 500px;" ++
                                     "border: 1px solid black; background-color: light-blue" ++
                                     "display: block;")]

displaySampleBlock :: MonadWidget t m => Int -> Dynamic t Info -> m ()
displaySampleBlock unusedKey dynInfo = mdo
    (boxEl,_) <- elDynAttr' "div" attrsDyn $ do
      display dynInfo
    mousePosE <- wrapDomEvent (R._el_element boxEl) (R.onEventName R.Drag) getMouseEventCoords
    pos <- holdDyn (0,0) mousePosE
    display pos
    attrsDyn <- forDyn pos $ \(b,c) ->
      Data.Map.fromList
      [("draggable", "true"),("class","countBin noselect")
      ,("style","width:" ++ show (b*0 + 30) ++ "px;" ++
        "background-color: hsl("++ show (b*0 + 5) ++ ",50%,50%);" ++
        "height: 30px; float: left; border: 1px solid black; position: relative;" ++
        "display:block; padding:.3em 0.5em; top:" ++ show (c) ++ "px; left:" ++ show(b) ++ "px;")]
    return ()

paletteEl :: MonadWidget t m => (Map String String) -> String -> m (R.Event t Info)
paletteEl attrs name = do
  (e,_) <- elAttr' "li" attrs $ text name
  return $ ( name <$ R.domEvent R.Click e)

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
              [ ("style", "fontsize: 10px;" ++
                "position: relative; float: left; text-decoration:none;" ++
                "display:block; padding:.5em 2em; background:#cde;" ++
                "border:1px solid #ccc; ")]
      ulAttrs = Data.Map.fromList
              [("style", "list-style: none; margin:0; padding:0; position: absolute")]
              
