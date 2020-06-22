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


splitPageWithAnnouncements :: MonadWidget t m => Editor t m a -> Editor t m a
splitPageWithAnnouncements child = divClass "pageSplitter" $ do
  r <- divClass "halfPage" $ child
  divClass "halfPage" announcementsWidget
  return r


-- compiledAnnouncements contains "compiled-in" news and announcements, which are part
-- of the client and call attention to new features, changes, etc. These are
-- merged with dynamic announcements from the server to form the total displayed
-- news and announcements.

compiledAnnouncements :: Map Day [TranslatableText]
compiledAnnouncements = fromList [
  (read "2020-06-22",[ fromList [
    (English,"This is a compiled announcement.")
    ]]),
  (read "2020-05-10",[ fromList [
    (English,"This is another compiled announcement.")
    ]])
  ]

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
    let xs = fmap (concat . mapWithKey  (\k a -> zip (repeat k) a)) annMap
    simpleList xs individualAnnouncement

  return ()

individualAnnouncement :: MonadWidget t m => Dynamic t (Day,TranslatableText) -> Editor t m ()
individualAnnouncement x = divClass "announcement" $ do
  dynText $ fmap (T.pack . show . fst) x
  text "+ "
  dynText =<< dynTranslatableText (fmap snd x)
