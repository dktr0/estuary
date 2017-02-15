
module Main where

import Reflex
import Reflex.Dom
import Estuary.Tidal.Types
import Estuary.Reflex.Utility
import Estuary.Widgets.Generic
import Estuary.Widgets.StackedPatterns
import Estuary.Widgets.PatternChain as P
import Estuary.Widgets.GeneralPattern as G -- for testing the Refactor of general container
import Control.Monad (liftM)
import qualified Sound.Tidal.Context as Tidal
import Estuary.WebDirt.Foreign
import Estuary.WebDirt.Stream
import Estuary.Page
import Estuary.Widgets.SpecificPattern
import Estuary.Widgets.WebDirt
import Data.Map
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  wd <- webDirt
  stream <- webDirtStream wd
  mainWidget $ do
    divClass "estuary" $ do
      newPage <- divClass "header" $ do
        divClass "logo" $ text "logo"
        divClass "webDirt" $ text "webDirt"
        newPage <- divClass "pageMenu" $ pageMenu
        divClass "hintArea" $ text "hintArea"
        return newPage
      divClass "page" $ do
        let newPage' = fmap (page webDirt) newPage
        widgetHold (page webDirt (snd (pages!!0))) newPage'

pageMenu :: (MonadWidget t m) => m (Event t (m (Dynamic t ParamPattern)))
pageMenu = do
  let pageNames = Prelude.map (fst) pages
  let pageList = zipWith (\x y -> (y,x)) pageNames ([0..]::[Int])
  let pageMap = constDyn $ fromList pageList
  dd <- dropdown 0 pageMap def
  return $ fmap (Prelude.map (snd) pages !!) $ _dropdown_change dd

pages = [
  ("ICLC Text Widget",widgetToPage $ iclcTextWidget EmptyPatternChain never),
  ("ICLC Stacked Patterns Widget",widgetToPage twoStackedPatterns),
  ("ICLC Fixed Widget", widgetToPage $ P.iclcFixedStruct EmptyPatternChain never),
  ("ICOAH", widgetToPage $ P.icoahWidget EmptyPatternChain never)
  ]

twoStackedPatterns :: MonadWidget t m => m (Dynamic t (StackedPatterns,Event t GenericSignal))
twoStackedPatterns = stackedPatternsWidget (StackedPatterns [EmptyPatternChain,EmptyPatternChain]) never

widgetToPage :: (MonadWidget t m, ParamPatternable p) => m (Dynamic t (p,a)) -> m (Dynamic t ParamPattern)
widgetToPage x = do
  y <- x
  mapDyn fst y

page :: (MonadWidget t m) => WebDirtStream -> m (Dynamic t ParamPattern) -> m ()
page webDirt x = do
  pattern <- x
  let evalEvent = updated pattern
  performEvent_ $ fmap (liftIO . webDirt) evalEvent
