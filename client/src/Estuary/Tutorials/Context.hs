module Estuary.Tutorials.Context where

import Reflex
import Reflex.Dom

import Estuary.Tutorials.Tutorial
import Estuary.Tutorials.IntroTidalText (introTidalText)

tutorials::MonadWidget t m => [Tutorial t m]
tutorials = [introTidalText]
