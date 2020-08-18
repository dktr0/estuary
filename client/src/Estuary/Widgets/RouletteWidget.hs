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
    let ensC = fmap ensembleC ctx
    let uHandle = fmap userHandle ensC -- Dynamic Text
    uHandle' <- sample $ current uHandle -- Text

    let lineUpBool' = fmap (lineUpBool uHandle') currentVal
    let dynAttrs = attrsLineUpButton <$> lineUpBool'

    let listOfRouletteButtons' = fmap leftmost listOfRouletteButtons -- Dynamic t (Event t (Roulette -> Roulette))
    let listOfRouletteButtons'' = switchDyn listOfRouletteButtons'  -- m (Event t (Roulette -> Roulette))
    let attrsRouletteButton' = attrsRouletteButton uHandle'  --  Map Text Text
    listOfRouletteButtons <- simpleList currentVal (rouletteButton attrsRouletteButton') -- m (Dynamic [Event t (Roulette -> Roulette)])

    lineUpButton' <- lineUpButton dynAttrs "+" (addHandleToList uHandle') -- (Event t (Roulette -> Roulette)

    currentVal <- holdUniqDyn $ currentValue x -- Dynamic t [Text]
    let currentValToBehaviour = current (currentValue x) -- Behaviour RouletteView
    let buttonEvs = mergeWith (.) [listOfRouletteButtons'', lineUpButton'] -- Event t (Roulette -> Roulette)
    let newValue = attachWith (flip ($)) currentValToBehaviour buttonEvs  -- Event t Roulette
    x <- returnVariable delta newValue
    return x

-- Dynamic t a (comes from the server, and only is the initial value and it also represents the udpates) -> Event t a (come from local user actions) -> Variable t Roulette
lineUpBool :: Text -> [Text] -> Bool
lineUpBool uHandle roulette
  |elem uHandle roulette = True
  |uHandle == "" = True
  |otherwise = False

rouletteButton :: MonadWidget t m =>  Map Text Text -> Dynamic t Text -> m (Event t (Roulette -> Roulette))
rouletteButton attrs label = do
  label' <- sample $ current label-- Text
  let r = removeHandleFromList $ label' -- <> "     " <> "x"  -- Dynamic t (Roulette -> Roulette)
  (element, _) <- elAttr' "div" attrs $ dynText $ label <> (constDyn " ") <> (constDyn "ⓧ")
  clickEv <- wrapDomEvent (_el_element element) (elementOnEventName Click) (mouseXY)
  let roulette = r <$ clickEv
  return roulette

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
--addHandleToList "luis" -- Roulette -> Roulette -- check if the name is in the list already and add if not.
-- Both of these func should be pure funcs.

-- e.g. deleteHandleFromL :: Text -> Roulette -> Roulette
-- deleteHandleFromL -- fail silently , i.e. return the existing list

removeHandleFromList :: Text -> Roulette -> Roulette
removeHandleFromList handle xs = Prelude.filter (\e -> e/=handle) xs
