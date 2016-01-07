import Reflex.Dom
import Reflex.Host.Class
import Reflex
import Control.Monad.Trans
import Data.Dependent.Sum
import Data.Monoid
import Data.Maybe
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.DOM.Types hiding (Event)
import GHCJS.DOM.EventM

main :: IO ()
main = mainWidget $ do
  header
  samples
  modifiers
  composing
  visualization
  footer
  countClicks

header :: MonadWidget t m => m ()
header = do
  el "div" $ do
    el "strong" $ do
      linkNewTab "https://github.com/d0kt0r0/estuary" "Estuary"
      text " a Projectional Interface for the "
      linkNewTab "https://github.com/tidalcycles/Tidal" "Tidal"
      text " language"
    el "p" $ do
      text "choose a sample to begin."

samples :: MonadWidget t m => m ()
samples = do
  el "div" $ do
    el "strong" $ do
      text "Samples"
    el "p" $ do
      text "sample stuff goes here"

modifiers :: MonadWidget t m => m ()
modifiers = do
  el "div" $ do
    el "strong" $ do
      text "Modifiers"
    el "p" $ do
      text "modifier stuff here"

composing :: MonadWidget t m => m ()
composing = do
  el "div" $ do
    el "strong" $ do
      text "Composing stuff here"
    el "p" $ do
      text "Also have to do the css class junk"

visualization :: MonadWidget t m => m ()
visualization = do
  el "div" $ do
    el "strong" $ do
      text "Visualizations"
    el "p" $ do
      text "visualization stuff here"

footer :: MonadWidget t m => m ()
footer = do
  el "div" $ do
    el "strong" $ do
      text "Footer"
    el "p" $ do
      text "gotta style this junk"

linkNewTab :: MonadWidget t m => String -> String -> m ()
linkNewTab href s =
  elAttr "a" ("href" =: href <> "target" =: "_blank") $ text s

countClicks :: MonadWidget t m => m ()
countClicks = do

  let initialCounters = Map.fromList [(i,()) | i <- [0..2 :: Int]]
      addCounter  cs  = Map.insert (length cs) () cs
      dropCounter cs  = case Map.maxView cs of
        Nothing      -> cs
        Just (_,cs') -> cs'

  allBins <- foldDyn ($)
             initialCounters
             (leftmost $ [dropBinButton, addBinButton])

  elClass "div" "allCounts" $ do

    listWithKey allBins $ \k oneBin -> do

      nClicks <- foldDyn (\() -> succ) (0 :: Int) (_el_clicked boxEl)

      attrsDyn <- forDyn nClicks $ \b ->
        Map.fromList
        [("class","countBin noselect")
        ,("style","height:" ++ show (30+b*3) ++ "px;" ++
                  "background-color: hsl("++
                  show (b*5) ++ ",50%,50%);")]

      (boxEl,_) <- elDynAttr' "div" attrsDyn $ do
        display nClicks

      return ()

  el "br" (return ())

  -- Turn links' () click events into functions to apply to the bin map
  dropBinButton <- (fmap (\() -> dropCounter) . _link_clicked) <$>
                   linkClass "Remove Bin" "reflexLink noselect"
  addBinButton  <- (fmap (\() -> addCounter)  . _link_clicked) <$>
                   linkClass "Add Bin" "reflexLink noselect"
  return ()
