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

data Sample = Bd | Bp | Cp
  deriving (Show, Eq)

main :: IO ()
main = mainWidget $ do
  elAttr "div" ("style" =: s) $ text "Estuary"
  d <- dropdown "" (constDyn samples) def
  setup
  countClicks
  where
    s = "font-size: 50px; margin-left: 155px; font-family: Helvetica; color: steelblue"

setup :: MonadWidget t m => m ()
setup = el "div" $ do
  text "hello"
  el "br" (return ())
  {- set up different divs -}

buttonDynAttr :: MonadWidget t m => Dynamic t (Map String String) -> m (Event t ())
buttonDynAttr attrs = do
  (e, _) <- elDynAttr' "button" attrs (text "")
  return $ domEvent Click e



countClicks :: MonadWidget t m => m ()
countClicks = mdo

  -- Setup the three initial counters
  let initialCounters = Data.Map.fromList [(i,()) | i <- [0..2 :: Int]]
      addCounter  cs  = Data.Map.insert (length cs) () cs
      dropCounter cs  = case Data.Map.maxView cs of
         Nothing      -> cs
         Just (_,cs') -> cs'

  allBins <- foldDyn ($)
             initialCounters
             (leftmost $ [dropBinButton, addBinButton])

  -- Create a div that encapsulates all the counters
  elClass "div" "allCounts" $ do

    -- Do for each bin? (look into this)
    listWithKey allBins $ \k oneBin -> mdo

      -- Increment the clicked counter by 1
      nClicks <- foldDyn (\() -> succ) (0 :: Int) (domEvent Click boxEl)

      -- Increase the size of the clicked div based on its counter value
      attrsDyn <- forDyn nClicks $ \b ->
        Data.Map.fromList
        [("class","countBin noselect")
        ,("style","width:" ++ show (30+b*3) ++ "px;" ++
                  "background-color: hsl("++
                  show (b*5) ++ ",50%,50%);")]

      -- Draw the new counter and display the number of clicks on it
      (boxEl,_) <- elDynAttr' "div" attrsDyn $ do
        display nClicks

      return ()

  -- line break
  el "br" (return ())

  -- Turn links' () click events into functions to apply to the bin map
  dropBinButton <- (fmap (\() -> dropCounter) . _link_clicked) <$>
                 linkClass "Remove Bin" "reflexLink noselect"
  addBinButton  <- (fmap (\() -> addCounter)  . _link_clicked) <$>
                 linkClass "Add Bin" "reflexLink noselect"
  return ()

samples = Data.Map.fromList [("bd","bd"), ("sn","sn"), ("cp","cp")]
