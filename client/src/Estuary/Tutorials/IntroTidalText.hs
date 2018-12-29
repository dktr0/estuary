{-# LANGUAGE OverloadedStrings #-}
module Estuary.Tutorials.IntroTidalText where

import Reflex
import Reflex.Dom

import qualified Data.IntMap.Strict as IM
import Data.Map as M
import Estuary.Tutorials.Tutorial
import Estuary.Types.Language

import Estuary.Types.Definition

-- miniTidalWidget
import Estuary.Types.Context
import Estuary.Types.Hint


introTidalText ::MonadWidget t m => Tutorial t m
introTidalText = Tutorial IntroTidalText introTidalTextWidget

introTidalTextWidget::MonadWidget t m => Dynamic t Context -> m (Dynamic t DefinitionMap, Event t Hint)
introTidalTextWidget ctx = elClass "div" "tutorial" $ do
  title $ labelWidget ctx $ M.fromList [(English, "Welcome to the introductory tutorial to TidalCycles (or MiniTidal)!")]
  labelWidget ctx $ M.fromList [(English,"This tutorial will cover some of the basics of making music with TidalCycles. MiniTidal is a subset of TidalCycles that supports most typical Tidal operations (but not all), but everything shown here (and anything that works with MiniTidal) will also work with TidalCycles.")]

  el "div" $ labelWidget ctx $ fromList [(English, "Lets make some sound! Click 'eval' below. You should here a simple \"bassdrum clap\" drum pattern. (hit 'silence' on the right to stop it)")]
  (v1,h1) <- miniTidalWidget ctx 1 1 "s \"bd cp\""

  el "div" $ labelWidget ctx $ fromList [(English, "In the text field above, we've specified a pattern of samples (specifically 'bd' and 'cp') that fill up a 'cycle' (which can be thought of sort of like musical bars if you like). Everything that appears within the quotes (\"\") divides a cycle into equal parts: the \"bd\" sample gets the first half of the cycle, and the \"cp\" gets the second half. ")]

  el "div" $ labelWidget ctx $ fromList [(English,"If we put a third element into our pattern we get a different rhythm:")]

  (v2,h2) <- miniTidalWidget ctx 1 2 "s \"bd cp hh\""

  el "div" $ labelWidget ctx $ fromList [(English,"Each sample gets 1/3rd of a cycle.")]

  el "div" $ labelWidget ctx $ fromList [(English,"A \"~\" placed in a Tidal pattern has a special designation as a 'rest'. So we can get rid of the 'hh' in the above example but preserve the rhythm:")]
  (v3,h3) <- miniTidalWidget ctx 1 3 "s \"bd cp ~\""

  el "div" $ labelWidget ctx $ fromList [(English,"Sometimes we want to subdivide a part of a cycle - for instance if we want 2 samples to play in the last half of a cycle instead of just one: ")]
  (v4, h4) <- miniTidalWidget ctx 1 4 "s \"bd [cp hh]\""

  el "div" $ labelWidget ctx $ fromList [(English,"Or if we want two samples to play at the same time we can enclose them in square brackets with a comma between them: ")]
  (v5, h5) <- miniTidalWidget ctx 1 5 "s \"bd [cp,hh]\""

  el "div" $ labelWidget ctx $ fromList [(English,"Both ideas together: ")]
  (v6, h6) <- miniTidalWidget ctx 1 5 "s \"bd [cp,hh casio]\""

  el "div" $ labelWidget ctx $ fromList [(English,"")]

  v <- dynList [v1, v2, v3, v4,v5,v6] >>= mapDyn IM.fromList
  let hints = leftmost [h1,h2,h3,h4,h5,h6]
  return (v, hints)
