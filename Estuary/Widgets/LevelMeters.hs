module Estuary.Widgets.LevelMeters where

import Reflex
import Reflex.Dom
import Control.Monad.IO.Class (liftIO)
import Data.Time

import Estuary.Types.Context
import qualified Estuary.WebDirt.SampleEngine as SampleEngine

levelMeterWidget :: MonadWidget t m => Dynamic t Context -> m ()
levelMeterWidget ctx = do
  t <- forDyn ctx $ \ctx' -> show (peakLevels ctx') ++ show (rmsLevels ctx')
  let t' = nubDyn t
  dynText t'

monitorWebDirtLevels :: (MonadWidget t m, SampleEngine.SampleEngine e)
  => UTCTime -> e -> m (Event t ContextChange)
monitorWebDirtLevels now e = do
  tick <- tickLossy (0.020::NominalDiffTime) now
  peaks <- performEvent $ fmap (liftIO . (\_ -> SampleEngine.peakLevels e)) tick
  rms <- performEvent $ fmap (liftIO . (\_ -> SampleEngine.rmsLevels e)) tick
  return $ mergeWith (.) [fmap setPeakLevels peaks,fmap setRmsLevels rms]
