-- Matthew Paine
-- January 28 2016
-- Estuary UI : Reflex/GHCJS front end for Tidal
-- My attempt at creating a drag and drop interface with Reflex and GHCJS
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           GHCJS.DOM.EventM (mouseClientXY)
import qualified GHCJS.DOM.EventM as GHCJS

--instance IsMouseEvent MouseEvent

data DragEvent = DragEnter | DragExit | DragDrop | DragDrag | DragEnd | DragClick | Empty
  deriving (Eq, Show)

-- Tuple template
tuple :: a -> b -> (a,b)
tuple x y = (x,y)

-- Function that implements the prevent default java action so that drop events can be detected.
stopAll :: GHCJS.IsEvent event => GHCJS.EventM event e ()
stopAll = do
  GHCJS.preventDefault
  GHCJS.stopPropagation
  return()

main :: IO ()
main = mainWidget $ do
  elAttr "div" ("style" =: s) $ text "Estuary"
  setup
  dragAndDrop
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

-- Implements the drag and drop functionality for the sample blocks.
dragAndDrop :: forall t m. R.MonadWidget t m => m ()
dragAndDrop = mdo

  -- initialize samples as individual objects
  let initialSamples = Data.Map.fromList [(i,()) | i <- [0 :: Int]]
      addSampleBlock  cs  = Data.Map.insert (length cs) () cs
      removeSampleBlock cs  = case Data.Map.maxView cs of
        Nothing      -> cs
        Just (_,cs') -> cs'

  allBlocks <- foldDyn ($)
               initialSamples
               (leftmost $ [addBlock, removeBlock])

  -- set name = to dropdown_value define dropdown attributes



  -- run tidal parser on current pattern get back timing info


  -- Container, set attribute to draggable so it can be dropped on.
  -- Need to track drop events on this class
  -- Need to track enter and leave events (Switch attributes)
  (container, _) <- elAttr' "div" conAttrs $ do

    -- Create new block (do for each)
    listWithKey allBlocks $ \k oneBlock -> mdo

      -- Track mouse position on drag event
      posE <- wrapDomEvent (R._el_element boxEl) (GHCJS.elementOndrag) getMouseEventCoords
      pos <- holdDyn (0,0) posE
      display pos

      nClicks <- foldDyn (\() -> succ) (0 :: Int) (R.domEvent Click boxEl)

      --name <- (_dropdown_value d)

      -- Set sample attributes based on Tidal feedback and position based on mouse position.
      attrsDyn <- forDyn nClicks $ \b ->
        Data.Map.fromList
        [("draggable", "true"),("class","countBin noselect")
        ,("style","width:" ++ show (30+b*3) ++ "px;" ++
          "background-color: hsl("++ show (b*5) ++ ",50%,50%);" ++
          "height: 30px; float: left; border: 1px solid black;" ++
          "display:block; padding:.3em 0.5em;")]

      --name <- return $ dropdownMenu

      -- Create a sampleBlock element
      (boxEl,_) <- elDynAttr' "div" attrsDyn $ do
        display nClicks

      -- concatonate block names

      return ()

  el "br" (return ())

  -- Turn links' () click events into functions to apply to the bin map
  -- Switch to dropdown
  -- Create new sampleblock on drop (track drops on the container) link to seperate function
  addBlock <- (fmap (\() -> addSampleBlock) <$> dropdownMenu)
  --addBlock <- (fmap (\() -> addSampleBlock) . _link_clicked) <$>
  --                linkClass "Add Sample" "reflexLink noselect"

  -- How to remove?
  removeBlock <- (fmap (\() -> removeSampleBlock) . _link_clicked) <$>
                     linkClass "Remove Sample" "reflexLink noselect"

  return ()

  where
    conAttrs = Data.Map.fromList [("style", "position: relative; top: 25px; height: 500px;" ++
                                   "border: 1px solid black; background-color: light-blue" ++
                                   "display: block;")]

{-
mousePosition :: (IsMouseEvent e, GHCJS.IsElement t) => GHCJS.EventM e t (Int, Int)
mousePosition = do
  (x, y) <- mouseClientXY
  return (x, y)
-}

--test = fst mousePosition

dropdownEl :: MonadWidget t m => (Map String String) -> String -> m (R.Event t ())
dropdownEl attrs name = do
  (e,_) <- elAttr' "li" attrs $ text name
  return $ (R.domEvent R.Click e)

dropdownMenu :: MonadWidget t m => m (R.Event t ())
dropdownMenu = do
    elAttr "ul" ulAttrs $ do
      bd <- dropdownEl liAttrs "bd"
      sn <- dropdownEl liAttrs "sn"
      arpy <- dropdownEl liAttrs "arpy"
      arp <- dropdownEl liAttrs "arp"
      hh <- dropdownEl liAttrs "hh"
      ht <- dropdownEl liAttrs "ht"
      cp <- dropdownEl liAttrs "cp"
      (return $ R.leftmost [bd,sn,arpy,arp,hh,ht,cp])
    where
      liAttrs = Data.Map.fromList
              [ ("style", "fontsize: 10px;" ++
                "position: relative; float: left; text-decoration:none;" ++
                "display:block; padding:.5em 2em; background:#cde;" ++
                "border:1px solid #ccc; bottom: 535px;")]
      ulAttrs = Data.Map.fromList
              [("style", "list-style: none; margin:0; padding:0; position: relative;")]

-- Set a dynamic div's attributes based on the type of event that fired
whichAttr :: DragEvent -> (Map String String)
whichAttr conState
         | conState == DragEnter = Data.Map.fromList
                               [("style", "border: 3px dotted black;" ++
                                 "position: absolute; left: 20px; top: 120px; width: 500px; height: 120px;")]
         | conState == DragExit = Data.Map.fromList
                               [("style", "border: 1px solid purple;" ++
                                 "position: absolute; left: 20px; top: 120px; width: 500px; height: 120px;")]
         | conState == DragDrop = Data.Map.fromList
                               [("style", "border: 2px solid purple;" ++
                                 "position: absolute; left: 20px; top: 120px; width: 500px; height: 120px;")]
         | otherwise     = Data.Map.fromList
                               [("style", "border: 1px solid purple;" ++
                                 "position: absolute; left: 20px; top: 120px; width: 500px; height: 120px;")]
