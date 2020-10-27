{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.Announcements (splitPageWithAnnouncements) where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)
import Data.Map
import qualified Data.Text as T
import Data.Time

import Estuary.Types.Context
import Estuary.Types.Language
import Estuary.Types.TranslatableText
import Estuary.Reflex.Utility
import Estuary.Widgets.Editor

-- compiledAnnouncements contains "compiled-in" news and announcements, which are part
-- of the client and call attention to new features, changes, etc. These are
-- merged with dynamic announcements from the server to form the total displayed
-- news and announcements.

compiledAnnouncements :: Map Day [TranslatableText]
compiledAnnouncements = fromList [
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


splitPageWithAnnouncements :: MonadWidget t m => Editor t m a -> Editor t m a
splitPageWithAnnouncements child = divClass "pageSplitter" $ do
  r <- divClass "halfPage" $ child
  divClass "halfPage" announcementsWidget
  return r


announcementsWidget :: MonadWidget t m => Editor t m ()
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

individualAnnouncement :: MonadWidget t m => Dynamic t (Day,TranslatableText) -> Editor t m ()
individualAnnouncement x = divClass "announcement" $ do
  dynText $ fmap (T.pack . show . fst) x
  text ": "
  dynText =<< dynTranslatableText (fmap snd x)
