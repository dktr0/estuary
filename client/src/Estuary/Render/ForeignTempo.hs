{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Render.ForeignTempo (ForeignTempo(..),toForeignTempo,showForeignTempo) where

-- | The ForeignTempo type is used to interface with (and is essentially defined by)
-- the purescript-tempi library. toForeignTempo can be used to convert Estuary's
-- normative Tempo type to a ForeignTempo.

import Data.Ratio
import GHCJS.DOM.Types hiding (Text)
import Estuary.Types.Tempo
import Data.Time.Clock.POSIX
import Data.Text (Text)

newtype ForeignTempo = ForeignTempo JSVal

instance PToJSVal ForeignTempo where pToJSVal (ForeignTempo x) = x

instance PFromJSVal ForeignTempo where pFromJSVal = ForeignTempo

foreign import javascript unsafe
  "{ freqNumerator: $1, freqDenominator: $2, time: $3, countNumerator: $4, countDenominator: $5 }"
  _newForeignTempo :: Int -> Int -> Double -> Int -> Int -> ForeignTempo

toForeignTempo :: Tempo -> ForeignTempo
toForeignTempo t = _newForeignTempo freqNumerator freqDenominator time countNumerator countDenominator
  where
    freqRatioInt = fromRational (freq t) :: Ratio Int
    freqNumerator = numerator freqRatioInt
    freqDenominator = denominator freqRatioInt
    time = realToFrac (utcTimeToPOSIXSeconds $ Estuary.Types.Tempo.time t) * 1000.0
    countRatioInt = fromRational (count t) :: Ratio Int
    countNumerator = numerator countRatioInt
    countDenominator = denominator countRatioInt
    
foreign import javascript unsafe
  "$1.freqNumerator.toString() + \" \" + $1.freqDenominator.toString() + \" \" + $1.time.toString() + \" \" + $1.countNumerator.toString() + \" \" + $1.countDenominator.toString()"
  showForeignTempo :: ForeignTempo -> Text 
