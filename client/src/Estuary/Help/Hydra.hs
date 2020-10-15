{-# LANGUAGE OverloadedStrings #-}
module Estuary.Help.Hydra where

import Reflex
import Reflex.Dom
import Data.Text
import GHCJS.DOM.EventM
import Estuary.Widgets.Generic
import Estuary.Reflex.Utility

hydraHelpFile :: MonadWidget t m => m ()
hydraHelpFile = divClass "languageHelpContainer" $ divClass "languageHelp" $ do
  about
  functionRef "out()"
-- sources
  functionRef "osc()"
  functionRef "solid()"
  functionRef "gradient()"
  functionRef "noise()"
  functionRef "shape()"
  functionRef "voronoi()"
  return ()

-- about
about :: MonadWidget t m => m ()
about = do
  divClass "about primary-color code-font" $ text  "Hydra is a live coding platform for visual sintesis based on video analog synthesizers. Hydra is an open-source project developed by Olivia Jack. Estuary contains a mini version of Hydra, running just the following functions."


exampleText :: MonadWidget t m => Text -> m ()

-- sources
exampleText "out()" = do
  el "div" $ text "solid().out()"
  el "div" $ text "solid().out(o2)"
  el "div" $ text "solid().brightness().out(o1)"
exampleText "osc()" = do
  el "div" $ text "osc().out()"
  el "div" $ text "osc(40,0.1,0.5).out()"
  el "div" $ text "osc(40,0.1,[2.0,0.5,1.0]).out()"
exampleText "solid()" = do
  el "div" $ text "solid().out()"
  el "div" $ text "solid(0.3,0,0.5).out()"
  el "div" $ text "solid([0.4,0.5],[0.0, 0.5],[0.1, 0.2]).out()"
exampleText "gradient()" = do
  el "div" $ text "gradient().out()"
  el "div" $ text "Colour gradient with one parameter: gradient(speed); default: gradient(0)"
exampleText "noise()" =
  el "div" $ text "noise().out()"
exampleText "shape()" =
  el "div" $ text "shape().out()"
exampleText "voronoi()" =
  el "div" $ text "voronoi().out()"


referenceText :: Text -> Text

referenceText "out()" = "sends a source to an output: o0, o1, o2, o3. Default output is o0"
-- sources
referenceText "osc()" = "Oscillating (continous) wave forms with three parameters: osc(fequency, sync, rgb-offset); default: osc(60,0.1,0)"
referenceText "solid()" = "Solid colour with four parameters: solid(red, green, blue, alpha; default: solid(0,0,0,1)"
referenceText "gradient()" = "Colour gradient with one parameter: gradient(speed); default: gradient(0)"
referenceText "noise()" = "Perlin (or tv) noise texture with two parameters: noise(scale, offset); deafult: noise(10,0.1)"
referenceText "shape()" = "Poligons with three parameters: shape(sides, radius, smoothing); default: shape(3,0.3,0.01)"
referenceText "voronoi()" = "Vornoi diagrams with three parameters: voronoi(scale, speed, blending); default: voronoi(5,0.3,0.3)"


----------------------

functionRef :: MonadWidget t m => Text -> m ()
functionRef x = divClass "helpWrapper" $ do
 switchToReference <- buttonWithClass' x
 exampleVisible <- toggle True switchToReference
 referenceVisible <- toggle False switchToReference
 hideableWidget exampleVisible "exampleText primary-color code-font" (exampleText x)
 hideableWidget referenceVisible "referenceText code-font" $ text (referenceText x)
 return ()
