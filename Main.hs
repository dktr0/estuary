
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
import Sound.Tidal.Context (ParamPattern)
import Estuary.WebDirt.Foreign
import Estuary.WebDirt.Stream
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
      newPage <- header
      divClass "page" $ do
        let firstPage = snd (pages!!0)
        let newPage' = fmap (snd . (pages !!)) newPage
        pattern <- liftM (joinDyn) $ widgetHold firstPage newPage'
        let patternEval = updated pattern
        performEvent_ $ fmap (liftIO . stream) patternEval

header :: (MonadWidget t m) => m (Event t Int)
header = divClass "header" $ do
  divClass "logo" $ text "estuary"
  divClass "webDirt" $ text "webDirt"
  newPageIndex <- divClass "pageMenu" $ do
    let pageNames = Prelude.map (fst) pages
    let pageList = zipWith (\x y -> (y,x)) pageNames ([0..]::[Int])
    let pageMap = constDyn $ fromList pageList
    menu <- dropdown 0 pageMap def
    return $ _dropdown_change menu
  divClass "hintArea" $ text "hintArea"
  return newPageIndex

widgetToPage :: (MonadWidget t m,ParamPatternable p) => m (Dynamic t (p,a)) -> m (Dynamic t ParamPattern)
widgetToPage x = x >>= mapDyn (toParamPattern . fst)

-- pages :: MonadWidget t m => [(String,m (Dynamic t ParamPattern))]
pages = [
  ("Text Only Widget",widgetToPage $ textPatternChain EmptyPatternChain never),
  ("ICLC Text Widget",widgetToPage $ iclcTextWidget EmptyPatternChain never),
  ("ICLC Stacked Patterns Widget",widgetToPage $ twoStackedPatterns),
  ("ICLC Fixed Widget",widgetToPage $ P.iclcFixedStruct EmptyPatternChain never),
  ("ICOAH",widgetToPage $ P.icoahWidget EmptyPatternChain never)
  ]
