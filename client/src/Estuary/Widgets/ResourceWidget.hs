{-# LANGUAGE RecursiveDo, JavaScriptFFI, OverloadedStrings #-}

module Estuary.Widgets.ResourceWidget where

import Reflex
import Reflex.Dom
import qualified Reflex.Dom.Widget.Basic as B
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Foldable as F
import Data.Map
import      Data.Fixed (mod')


import Estuary.Types.Resources
import Estuary.Types.Scope

-- examples
-- aListOfVideoFiles :: ResourceMap VideoMeta --resourcesWidget
-- aListOfVideoFiles = ResourceMap {unResourceMap = (Data.Map.fromList [
--                           ("butterflies", (S.fromList [sampleVideoMedia, sampleVideoMedia'])),
--                           ("cats", (S.fromList [sampleVideoMedia, sampleVideoMedia'])),
--                           ("dogs", (S.fromList [sampleVideoMedia, sampleVideoMedia'])),
--                           ("landscapes", (S.fromList [sampleVideoMedia, sampleVideoMedia']))
--                           ])}
--
-- aListOfAudioFiles :: ResourceMap AudioMeta
-- aListOfAudioFiles = ResourceMap {unResourceMap = (Data.Map.fromList [
--                           ("arpy", (S.fromList [sampleAudioMedia, sampleAudioMedia'])),
--                           ("bd", (S.fromList [sampleAudioMedia, sampleAudioMedia'])),
--                           ("hh", (S.fromList [sampleAudioMedia, sampleAudioMedia'])),
--                           ("moog", (S.fromList [sampleAudioMedia, sampleAudioMedia']))
--                           ])}
--
-- aListOfImageFiles :: ResourceMap ImageMeta
-- aListOfImageFiles = ResourceMap {unResourceMap = (Data.Map.fromList [
--                           ("arpy", (S.fromList [sampleImageMedia, sampleImageMedia'])),
--                           ("bd", (S.fromList [sampleImageMedia, sampleImageMedia'])),
--                           ("hh", (S.fromList [sampleImageMedia, sampleImageMedia'])),
--                           ("moog", (S.fromList [sampleImageMedia, sampleImageMedia']))
--                           ])}
--
-- sampleImageMedia' :: Resource ImageMeta
-- sampleImageMedia' = Resource {resourceGroup = "landscapes", resourceFileName = "landscape0", resourceFileSize = 300, resourceMeta = ImageMeta {imageResolution = (800,1900), imageAspectRatio = (NineOverSixteen)}, resourceTags = S.fromList ["image", "landscape"], resourceScope = Ensemble}
--
-- sampleImageMedia :: Resource ImageMeta
-- sampleImageMedia = Resource {resourceGroup = "sky", resourceFileName = "sky1", resourceFileSize = 300, resourceMeta = ImageMeta {imageResolution = (800,1900), imageAspectRatio = (NineOverSixteen)}, resourceTags = S.fromList ["image", "landscape"], resourceScope = Ensemble}
--
--
-- sampleVideoMedia :: Resource VideoMeta
-- sampleVideoMedia = Resource {resourceGroup = "butterflies", resourceFileName = "butterflies0", resourceFileSize = 100, resourceMeta = VideoMeta {videoDuration = 195.0, videoResolution = (400,600), videoAspectRatio = FourOverThree}, resourceTags = S.fromList ["video", "butterflies"], resourceScope = Public}
--
-- sampleVideoMedia' :: Resource VideoMeta
-- sampleVideoMedia' = Resource {resourceGroup = "cats", resourceFileName = "cats1", resourceFileSize = 300, resourceMeta = VideoMeta {videoDuration = 3.25, videoResolution = (800,1900), videoAspectRatio = (Custom 2.0)}, resourceTags = S.fromList ["video", "cats"], resourceScope = Ensemble}
--
--
-- sampleAudioMedia :: Resource AudioMeta
-- sampleAudioMedia = Resource {resourceGroup = "arpy", resourceFileName = "arpy0", resourceFileSize = 100, resourceMeta = AudioMeta {audioDuration = 195.0}, resourceTags = S.fromList ["audio", "arpy"], resourceScope = Private}
--
-- sampleAudioMedia' :: Resource AudioMeta
-- sampleAudioMedia' = Resource {resourceGroup = "bd", resourceFileName = "bd1", resourceFileSize = 100, resourceMeta = AudioMeta {audioDuration = 300.0}, resourceTags = S.fromList ["audio", "bd"], resourceScope = Ensemble}
--
-- -- a function to show audio Duration
-- showAudioDuration ::  Resource AudioMeta -> T.Text
-- showAudioDuration a = T.pack $ humanReadableDuration $ (audioDuration (resourceMeta a))
--
-- -- a function to show video duration
-- showVideoDuration ::  Resource VideoMeta -> T.Text
-- showVideoDuration x = do
--  let a = humanReadableDuration $  (videoDuration (resourceMeta x))
--  T.pack $ a
--
-- -- a function to show video Shape
-- showVideoShape ::  Resource VideoMeta -> T.Text
-- showVideoShape x = T.pack $ show $ videoAspectRatio (resourceMeta x)
--
-- -- a function to show video image
-- showImageShape ::  Resource ImageMeta -> T.Text
-- showImageShape a = T.pack $ show $ imageAspectRatio (resourceMeta a)
--
-- -- a function to show the resource tags
-- showTags :: Resource a -> T.Text
-- showTags a = concatTags $ F.toList (resourceTags a) --List
--
-- -- a helper function for showTags
-- concatTags :: [T.Text] -> T.Text
-- concatTags [] = ""
-- concatTags [x] =  x
-- concatTags (x:xs) = T.concat ["<", x, ", ", concatTags xs, ">"]
--
-- -------- helper functions for showing Duration, adapted from Data.Duration -----------------------
-- ms :: Double
-- ms = 1.0
--
-- oneSec :: Double
-- oneSec= 1.0 -- * ms
--
-- oneMinute :: Double
-- oneMinute = 60.0 * oneSec
--
-- oneHour :: Double
-- oneHour = 60.0 * oneMinute
--
-- getMs :: Double -> Double
-- getMs n =  (/) n ms
--
-- getSeconds :: Double -> Int
-- getSeconds n = floor $ (/) n  oneSec
--
-- getMinutes :: Double -> Int
-- getMinutes n =  floor $ (/) n  oneMinute
--
-- getHours :: Double -> Int
-- getHours n = floor $ (/) n oneHour
--
-- humanReadableDuration :: Double -> String
-- humanReadableDuration n
--   | n < oneSec = let mi = getMs      n in if mi > 0 then show mi ++ "ms" else ""
--   | n < oneMinute = let s  = getSeconds n in if s  > 0 then show s  ++ "s" else ""
--   | n < oneHour   = let m  = getMinutes n in if m  > 0 then show  m  ++ " min " ++ (show $ floor (((n `mod'` oneMinute) /oneMinute) * 100))  ++ "s" else ""
--   | otherwise   = let h  = getHours   n in if h  > 0 then show h  ++ " hours " ++ (show $ floor (((n `mod'` oneHour) /oneHour) * 100))  ++ "min "  ++ (show $ floor (((n `mod'` oneMinute) /oneMinute) * 100))  ++ "s" else ""
--   -- | n < year   = let d  = getDays    n in if d  > 0 then show d  ++ " days " ++ humanReadableDuration (n `mod'` day) else ""
--
-- -- a widget to display one dyamic audio resources
-- audioResourceWidgetDyn :: MonadWidget t m => Dynamic t (Resource AudioMeta) -> m ()
-- audioResourceWidgetDyn a = divClass "resourceWidget" $ do
--   file' <- fmap resourceFileName a
--   dur <-  fmap showAudioDuration a
--   tags' <-  fmap showTags a
--   scope' <- fmap (T.pack . show . resourceScope) a
--   divClass "resourceName" $ dynText file'
--   divClass "resourceDur" $ dynText dur
--   divClass "resourceTags" $ dynText tags'
--   divClass "resourceScope" $ dynText scope'
--
-- -- a widget to display one dyamic video resources
-- videoResourceWidgetDyn :: MonadWidget t m => Dynamic t (Resource VideoMeta) -> m ()
-- videoResourceWidgetDyn a = divClass "resourceWidget" $ do
--   file' <- fmap resourceFileName a
--   dur <- fmap showVideoDuration a
--   shape <- fmap showVideoShape a
--   tags' <- fmap showTags a
--   scope' <-  fmap (T.pack . show . resourceScope) a
--   divClass "resourceName" $ dynText file'
--   divClass "resourceDur" $ dynText dur
--   divClass "resourceShape" $ dynText shape
--   divClass "resourceTags" $ dynText tags'
--   divClass "resourceScope" $ dynText scope'
--
--
-- -- a widget to display one dyamic image resource
-- imageResourceWidgetDyn :: MonadWidget t m => Dynamic t (Resource ImageMeta) -> m ()
-- imageResourceWidgetDyn a = divClass "resourceWidget" $ do
--   file' <- fmap resourceFileName a
--   shape <- fmap showImageShape a
--   tags' <- fmap showTags a
--   scope' <-  fmap (T.pack . show . resourceScope) a
--   divClass "resourceName" $ dynText file'
--   divClass "resourceShape" $ dynText shape
--   divClass "resourceTags" $ dynText tags'
--   divClass "resourceScope" $ dynText scope'

-- a widget to display a list of dyamic audio resources
-- audioResources :: MonadWidget t m => Dynamic t (ResourceMap AudioMeta) -> m (Dynamic t [()])
-- audioResources vs = divClass "resourceWidgetContainer code-font" $ do
--   divClass "resourceGroup" $ text $ T.pack "Audio Resources \n"
--   divClass "resourceLabels" $ do
--     divClass "resourceName" $ text $ T.pack $ "Name \n"
--     divClass "resourceDur" $ text $ T.pack $ "Duration \n"
--     divClass "resourceTags" $ text $ T.pack $ "Tags \n"
--     divClass "resourceScope" $ text $ T.pack $  "Scope \n"
--   vs' <- fmap resourceList vs
--   B.simpleList vs' $ \v -> audioResourceWidgetDyn v --m ()

  -- a widget to display a list of dyamic video resources
-- videoResources :: MonadWidget t m => Dynamic t (ResourceMap VideoMeta) -> m (Dynamic t [()])
-- videoResources vs = divClass "resourceWidgetContainer code-font" $ do
--   divClass "resourceGroup" $ text $ T.pack "Video Resources \n"
--   divClass "resourceLabels" $ do
--     divClass "resourceName" $ text $ T.pack $ "Name \n"
--     divClass "resourceDur" $ text $ T.pack $ "Duration \n"
--     divClass "resourceShape" $ text $ T.pack $ "Shape \n"
--     divClass "resourceTags" $ text $ T.pack $ "Tags \n"
--     divClass "resourceScope" $ text $ T.pack $  "Scope \n"
--   vs' <- fmap resourceList vs
--   B.simpleList vs' $ \v -> videoResourceWidgetDyn v --m ()

-- a widget to display a list of dyamic image resources
-- imageResources :: MonadWidget t m => Dynamic t (ResourceMap ImageMeta) -> m (Dynamic t [()])
-- imageResources vs = divClass "resourceWidgetContainer code-font" $ do
--   divClass "resourceGroup" $ text $ T.pack "Image Resources \n"
--   divClass "resourceLabels" $ do
--     divClass "resourceName" $ text $ T.pack $ "Name \n"
--     divClass "resourceShape" $ text $ T.pack $ "Shape \n"
--     divClass "resourceTags" $ text $ T.pack $ "Tags \n"
--     divClass "resourceScope" $ text $ T.pack $  "Scope \n"
--   vs' <- fmap resourceList vs
--   B.simpleList vs' $ \v -> imageResourceWidgetDyn v --m ()
