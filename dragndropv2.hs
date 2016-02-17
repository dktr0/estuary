-- Matthew Paine
-- January 28 2016
-- Estuary UI : Reflex/GHCJS front end for Tidal
-- My attempt at creating a drag and drop interface with Reflex and GHCJS
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ExistentialQuantification #-}

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

dragAndDrop :: MonadWidget t m => m ()
countClicks = mdo

  -- initialize samples as individual objects
  let initialSamples = Map.fromList [(i,()) | i <- [0 :: Int]]
      addSampleBlock  cs  = Map.insert (length cs) () cs
      removeSampleBlock cs  = case Map.maxView cs of
        Nothing      -> cs
        Just (_,cs') -> cs'

  -- set name = to dropdown_value define dropdown attributes
  d <- dropdown "bd" (constDyn samples) def

  -- run tidal parser on current pattern get back timing info


  allBlocks <- foldDyn ($)
              initialSamples
              (leftmost $ [addSampleBlock, removeSampleBlock])

  -- Container, set attribute to draggable so it can be dropped on.
  -- Need to track drop events on this class
  -- Need to track enter and leave events (Switch attributes)
  elClass "div" "allCounts" $ do

    -- Create new block (do for each)
    listWithKey allBlocks $ \k oneBlock -> mdo

      -- Track mouse position on drag event
      nClicks <- foldDyn (\() -> succ) (0 :: Int) (_el_clicked boxEl)

      name <- (_dropdown_value d)

      -- Set sample attributes based on Tidal feedback and position based on mouse position.
      attrsDyn <- forDyn nClicks $ \b ->
        Map.fromList
        [("class","countBin noselect")
        ,("style","height:" ++ show (30+b*3) ++ "px;" ++
                  "background-color: hsl("++
                  show (b*5) ++ ",50%,50%);")]

      -- Create a sampleBlock element
      (boxEl,_) <- elDynAttr' "div" attrsDyn $ do
        display nClicks

      -- concatonate block names


      return ()


  el "br" (return ())

  -- Turn links' () click events into functions to apply to the bin map
  -- Switch to dropdown
  -- Create new sampleblock on drop (track drops on the container)
  addSampleBlock <- (fmap (\() -> addSampleBlock) . _link_clicked) <$>
                     linkClass "Add Sample" "reflexLink noselect"
  -- How to remove?
  removeSampleBlock <- (fmap (\() -> removeSampleBlock) . _link_clicked) <$>
                     linkClass "Remove Sample" "reflexLink noselect"

  -- List of tidal samples
  samples = Map.fromList [("bd", "bd"), ("bp", "bp"), ("sn", "sn"), ("arpy", "arpy")]

  return ()

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
