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


