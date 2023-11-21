{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Estuary.Widgets.TapTempo (tapTempoWidget) where

import Reflex
import Reflex.Dom
import Data.Time
import Control.Monad.IO.Class (liftIO,MonadIO)
import TextShow
import Data.Text (Text)
import Data.Tempo
import Control.Monad.Fix

import qualified Estuary.Types.Term as Term
import Estuary.Widgets.W
import Estuary.Widgets.Reflex


slowestBPM :: NominalDiffTime
slowestBPM = 30

fastestBPM :: NominalDiffTime
fastestBPM = 190

tooSlowThreshold :: NominalDiffTime
tooSlowThreshold = 60 / slowestBPM

tooFastThreshold :: NominalDiffTime
tooFastThreshold = 60 / fastestBPM


type TapState = [UTCTime]

averageTimeDiff :: TapState -> NominalDiffTime
averageTimeDiff [] = tooSlowThreshold
averageTimeDiff (_:[]) = tooSlowThreshold
averageTimeDiff ts = (foldl (+) 0 $ zipWith diffUTCTime (init ts) (tail ts)) / realToFrac (length ts - 1)

updateTapState :: TapState -> UTCTime -> (Maybe TapState, Maybe (NominalDiffTime, UTCTime))
updateTapState [] t = (Just [t], Nothing)
updateTapState ts t
  | diffUTCTime t (head ts) > tooSlowThreshold = (Just [t], Nothing)
  | diffUTCTime t (head ts) < tooFastThreshold = (Just [t], Nothing)
  | length ts < 8 = (Just (t:ts), Nothing)
  | otherwise = (Just [], Just (averageTimeDiff ts, t))

  
tapTempoWidget :: (Reflex t, DomBuilder t m, PerformEvent t m, MonadHold t m, MonadFix m, MonadIO (Performable m), PostBuild t m) => W t m ()
tapTempoWidget = do
  b <- dynButton =<< term Term.Tap
  tapEvTime <- performEvent $ fmap (const $ liftIO getCurrentTime) b
  tapsCompleteEv <- mapAccumMaybe_ updateTapState [] tapEvTime
  t <- tempo
  setTempo $ attachWith calculateNewTempo (current t) tapsCompleteEv
  

calculateNewTempo :: Tempo -> (NominalDiffTime, UTCTime) -> Tempo
calculateNewTempo oldTempo (ndt, t) = Tempo { freq = f, time = t, Data.Tempo.count = c }
  where
    f = realToFrac $ 1 / (ndt * 4)
    c = realToFrac $ ceiling $ timeToCount oldTempo t


