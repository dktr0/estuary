{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.Announcements (splitPageWithAnnouncements) where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Control.Monad.Fix (MonadFix)
import Data.Map
import qualified Data.Text as T
import Data.Time

import Estuary.Types.Context
import Estuary.Types.Language
import Estuary.Types.TranslatableText
import Estuary.Widgets.Reflex
import Estuary.Widgets.W

-- compiledAnnouncements contains "compiled-in" news and announcements, which are part
-- of the client and call attention to new features, changes, etc. These are
-- merged with dynamic announcements from the server to form the total displayed
-- news and announcements.

compiledAnnouncements :: Map Day [TranslatableText]
compiledAnnouncements = fromList [
  (read "2022-01-21",[ fromList [
    (English,"Estuary now contains version 0.4.0 of Punctual, with support for video files as textures, optimized fragment shader generation, combinatorial semantics in many operations with multi-channel signals, and numerous other changes.")
    ]]),
  (read "2021-12-11",[ fromList [
    (English,"Estuary now supports multichannel audio output. Use !maxAudioOutputs at the terminal to query channels available in the browser, !audioOutputs to query currently used channels, and !setAudioOutputs [someNumber] to switch to a specific number of audio outputs.")
    ]]),
  (read "2021-11-18",[ fromList [
    (English,"Estuary now uses version 1.7.8 of Tidal, and MiniTidal now supports stutter, quantise, rangex, wedge, timeCat/timecat, chunk', lindenmayer, step, step', steps, spreadf, spreadChoose/spreadr, selectF, striateBy, perlinWith, perlin2, perlin2With, <>, and stutWith")
    ]]),
  (read "2021-02-12",[ fromList [
    (English,"Estuary now uses version 1.7.1 of Tidal (and MiniTidal now supports silent, stitch, stripe, fix, unfix, contrast, chooseBy, wchoose, wchooseBy, trigger, ur, layer, interlace).")
    ]]),
  (read "2020-12-23",[ fromList [
    (English,"There is now a Discord server that is the main place for help, discussion, etc about Estuary. All welcome! Sign-up at the following link (note that it's recommended to make an account on discord.com first): https://discord.gg/snvFzkPtFr")
    ]]),
  (read "2020-12-03",[ fromList [
    (English,"Estuary turns five! The 5th anniversary of the first commit to the Estuary code base will be celebrated with a day of performances, workshops, and talks, hosted by the Networked Imagination Laboratory. Full details at nil.mcmaster.ca soon!")
    ]]),
  (read "2020-11-19",[ fromList [
    (English,"Estuary now includes framerate-limiting by default on generative visuals, which reduces CPU usage measurably in most cases. The limit can be changed or removed in Settings (click the ? button in the top right). MiniTidal now has support for juxcut, juxBy, jux4, jux', juxcut', id and the composition operator (.).")
    ]]),
  (read "2020-09-27",[ fromList [
    (English,"Estuary now uses version 1.6.2 of Tidal (and MiniTidal now supports scale, toScale, arpeggiate, arp, weave, weaveWith, ghost, inside, outside, and inv).")
    ]]),
  (read "2020-06-19",[ fromList [
    (English,"Configuration settings like turning Canvas on/off, selecting audio dynamics presets, etc are now accessed by clicking the ? button on the top right, then clicking on the settings tab. The settings can be hidden again by clicking on the ? again.")
    ]]),
  (read "2020-06-16",[ fromList [
    (English,"Estuary now uses version 1.6.0 of Tidal to provide MiniTidal functionality.")
    ]]),
  (read "2020-06-07",[ fromList [
    (English,"Estuary now includes the latest version of Punctual, with support for non-band-limited oscillators (lfsqr,lftri,lfsaw) and the 'step' function for sequencing.")
    ]]),
  (read "2020-06-01",[ fromList [
    (English,"Estuary now supports tempo changes from the terminal. Use !setcps or !setbpm followed by a number to immediately change the tempo. !setcps sets the tempo in cycles/bars per seconds. !setbpm sets the tempo in beats per minute (based on the model of 4 presumed beats per cycle/bar).")
    ]])
  ]


splitPageWithAnnouncements :: (DomBuilder t m, Monad m, PostBuild t m, MonadHold t m, MonadFix m) => W t m a -> W t m a
splitPageWithAnnouncements child = divClass "pageSplitter" $ do
  r <- divClass "halfPage" $ child
  divClass "halfPage" announcementsWidget
  return r


announcementsWidget :: (DomBuilder t m, Monad m, PostBuild t m, Reflex t, Adjustable t m, MonadHold t m, MonadFix m) => W t m ()
announcementsWidget = divClass "announcements" $ do

  divClass "announcement" $ do
    dynText =<< (translatableText $ fromList [
      (English,"About This Estuary Server: ")
      ])
    dynText =<< dynTranslatableText =<< (fmap aboutThisServer <$> context)

  divClass "announcement" $ do
    dynText =<< (translatableText $ fromList [
      (English,"News and Announcements")
      ])

  do
    ctx <- context
    serverAnnouncements <- holdUniqDyn $ fmap announcements ctx
    let annMap = fmap (unionWith (++) compiledAnnouncements) serverAnnouncements
    let xs = fmap (reverse . concat . mapWithKey  (\k a -> zip (repeat k) a)) annMap
    simpleList xs individualAnnouncement

  return ()

individualAnnouncement :: (DomBuilder t m, PostBuild t m, Reflex t, MonadFix m, MonadHold t m) => Dynamic t (Day,TranslatableText) -> W t m ()
individualAnnouncement x = divClass "announcement" $ do
  dynText $ fmap (T.pack . show . fst) x
  text ": "
  dynText =<< dynTranslatableText (fmap snd x)
