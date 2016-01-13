{-# LANGUAGE RecursiveDo #-}

import          Control.Monad
import          Control.Monad.IO.Class
import          Data.Default
import          Data.Text (Text)
import          Data.Array
import          Data.Map (Map, fromList, maxView, insert)
import qualified Data.Text as T

{-import          JavaScript.JQuery hiding (Event)-}

import          Reflex
import          Reflex.Dom
import          Reflex.Dom.Widget.Basic

type Position = (Int, Int)

data Sample = Bd | Bp | Cp
  deriving (Show, Eq)

data Blocks = Empty | Full
  deriving (Show, Eq, Generic, NFData)

squares :: [Position]
squares = [(x, y) | y <- [1..8], x <- [1..8]]

main :: IO ()
main = mainWidget $ do
  elAttr "div" ("style" =: s) $ text "Estuary"
  -- d <- dropdown "" (constDyn samples) def
  setup
  sampleWidget
  blockDiagram
  where
    s = "font-size: 50px; margin-left: 155px; font-family: Helvetica; color: steelblue"

setup :: MonadWidget t m => m ()
setup = el "div" $ do
  text "hello"
  el "br" (return ())
  {- set up different divs -}

buttonAttr :: MonadWidget t m => (Map String String) -> String -> m ()
buttonAttr attrs name = do
  (e, _) <- elAttr' "button" attrs (text name)
  (return())

buttonDynAttr :: MonadWidget => Dynamic t (Map String String) -> m (Event t ())
buttonDynAttr dynAttrs = do
  (e, _) <- elDynAttr' "button" dynAttrs (text "")
  return $ (domEvent DragEnter e | domEvent Dragleave e)

-- Report back name when dropped
-- Only drop when over emptySpace
-- Switch attrs when dropped
-- Report back name

sampleBlock :: MonadWidget t m => String -> m ()
sampleBlock name = do
  rec b <- buttonAttr attrs name
  (return())
  where
    attrs = Data.Map.fromList
            [("draggable", "true"),
             ("class", "sampleBlock"),
             ("style", "fontsize: 10px;" ++
              "font-family: Helvetica;" ++
              "background-color: steelblue")]

sampleWidget :: MonadWidget t m => m ()
sampleWidget = elClass "div" "sampleWidget" $ do
  sampleBlock "bd"
  sampleBlock "bp"
  sampleBlock "sn"

-- Highlight when dragged over
-- Report back coordinates
emptySpace :: MonadWidget t m => Position -> m (Event t Position)
emptySpace coors = do
  rec b <- buttonDynAttr dynAttrs
  dynAttrs <- mapDyn (case {-dragEnter dragLeave -})
    Enter -> mkStyle "purple"
    Leave -> mkStyle "green"
  return $ fmap (const (coors)) b
  where
    mkStyle c = Data.Map.fromList
    [ ("style", "border: 1px solid " ++ c ++ ";")]

-- Make grid of empty spaces (Othello)
blockDiagram :: MonadWidget t m => m ()
blockDiagram = elClass "div" "blockDiagram" $ do
  rec rows <- mapM row [1..8]
  (return())

row :: MonadWidget t m => Int -> m [()]
row n = el "div" $
  mapM (emptySpace) (take 8 . drop 8 (8 * (n-1)) $ squares)

-- Thoughts:
-- block diagram returns list of event t Strings
-- The list is the name of each dropped sample ordered by array position
-- convert this to [dynamic t Strings] in Setup
-- concat this list of dynamic t Strings into the pattern
-- run this pattern through tidal to obtain feedback
-- switch dynamic attributes of sample widgets based on feedback.

{-
countClicks :: MonadWidget t m => m ()
countClicks = mdo

  -- Create a div that encapsulates all the counters
  elClass "div" "allCounts" $ do

      -- Draw the new counter and display the number of clicks on it
      (bdEl,_) <- elDynAttr' "div" attrs $ do
      text "bd"

      -- Increment the clicked counter by 1
      boolClicked <- foldDyn (\() -> succ) (0 :: Int) $ (domEvent Click bdEl)
      --foldDyn dragging (domEvent Drag boxEl)

      -- Increase the size of the clicked div based on its counter value
      where attrs = do attrsDyn <- forDyn boolClicked $ \b ->
        Data.Map.fromList
        [("class","countBin noselect")
        ,("draggable","true"),
         ("style","width:" ++ show (30+b*3) ++ "px;" ++
                  "background-color: hsl("++
                  show (b*50) ++ ",50%,50%);")]
      return ()

  -- line break
  el "br" (return ())
-}

samples = Data.Map.fromList [("bd","bd"), ("sn","sn"), ("cp","cp")]
