{-# LANGUAGE OverloadedStrings #-} {-# LANGUAGE RecursiveDo #-}

module Estuary.Widgets.RouletteWidget where

import Reflex
import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T
import TextShow
import Data.Time
import GHCJS.DOM.EventM

-- import Data.Tuple.Select
import Control.Monad.Trans (liftIO)
import Data.Map.Strict
import Control.Monad
import Estuary.Types.Context
import Estuary.Types.EnsembleC
import Estuary.Types.Ensemble
import Estuary.Types.EnsembleRequest
import Estuary.Types.Participant
import Estuary.Widgets.Editor
import Estuary.Widgets.Generic
import Estuary.Types.Definition
import Estuary.Types.Variable

import qualified Estuary.Types.Term as Term

rouletteWidget :: MonadWidget t m => Dynamic t Roulette -> Editor t m (Variable t Roulette)
rouletteWidget delta = divClass "rouletteContainer" $ mdo

    ctx <- context
    initialHandle <- sample $ current $ fmap (userHandle . ensembleC) ctx

    let attrsRouletteButton' = attrsRouletteButton initialHandle
    listOfRouletteButtons <- simpleList currentVal (rouletteButton attrsRouletteButton')
    let deleteEv = switchDyn $ fmap leftmost listOfRouletteButtons

    let dynAttrs = attrsLineUpButton <$> fmap (currentlyInLine initialHandle) currentVal
    lineupEv <- lineUpButton dynAttrs "+" (addHandleToList initialHandle)

    let editEvs = mergeWith (.) [deleteEv,lineupEv]
    let newValue = traceEvent "newValue" $ attachWith (flip ($)) (current currentVal) editEvs
    x <- returnVariable delta newValue
    currentVal <- holdUniqDyn $ currentValue x
    return x

currentlyInLine :: Text -> [Text] -> Bool
currentlyInLine uHandle roulette
  |elem uHandle roulette = True
  |uHandle == "" = True
  |otherwise = False

rouletteButton :: MonadWidget t m =>  Map Text Text -> Dynamic t Text -> m (Event t (Roulette -> Roulette))
rouletteButton attrs label = do
  (element, _) <- elAttr' "div" attrs $ dynText $ label <> (constDyn " ") <> (constDyn "ⓧ")
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Click) (mouseXY)
  return $ attachWith (\x _ -> removeHandleFromList x) (current label) clickEv
  
attrsRouletteButton :: Text -> Map Text Text
attrsRouletteButton uhandle
  | uhandle == "" = "class" =: "rouletteButtons ui-buttons code-font" <> "style" =: "cursor: not-allowed; pointer-events: none;"
  | otherwise = "class" =: "rouletteButtons ui-buttons code-font"

lineUpButton ::  MonadWidget t m => Dynamic t (Map Text Text) -> Text -> (Roulette -> Roulette) -> m (Event t (Roulette -> Roulette))
lineUpButton attrs label r = do
  (element, _) <- elDynAttr' "div" attrs $ text label
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Click) (mouseXY)
  let roulette = r <$ clickEv
  return roulette

attrsLineUpButton :: Bool -> Map Text Text
attrsLineUpButton b = "class" =: "lineUpButton ui-buttons other-borders code-font" <> "style" =: (pevents b <> cursor b <> colour b)
  where
    pevents False  = "pointer-events: auto; "
    pevents True = "pointer-events: none; "
    cursor False = "cursor: pointer; "
    cursor True = "cursor: not-allowed; "
    colour False =  "color:var(--primary-color)"
    colour True =  "color: var(--secondary-color)"

rouletteToRoulette :: Dynamic t Roulette -> Dynamic t Roulette
rouletteToRoulette xs = xs

addHandleToList :: Text -> Roulette -> Roulette
addHandleToList uHandle roulette
  |elem uHandle roulette = roulette
  |otherwise = (++) roulette [uHandle]

removeHandleFromList :: Text -> Roulette -> Roulette
removeHandleFromList handle xs = Prelude.filter (\e -> e/=handle) xs
