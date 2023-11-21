{-# LANGUAGE JavaScriptFFI #-}

module Estuary.Render.ForeignTempo (ForeignTempo(..), toForeignTempo, toForeignTempoDebug, showForeignTempo) where

-- | The ForeignTempo type is used to interface with (and is essentially defined by)
-- the purescript-tempi library. toForeignTempo can be used to convert Estuary's
-- normative Tempo type to a ForeignTempo.

import Data.Ratio
import GHCJS.DOM.Types hiding (Text)
import Estuary.Types.Tempo
import Data.Time.Clock.POSIX
import Data.Text (Text, pack)

newtype ForeignTempo = ForeignTempo JSVal

instance PToJSVal ForeignTempo where pToJSVal (ForeignTempo x) = x

instance PFromJSVal ForeignTempo where pFromJSVal = ForeignTempo

foreign import javascript unsafe
  "{ freqNumerator: BigInt($1), freqDenominator: BigInt($2), time: $3, countNumerator: BigInt($4), countDenominator: BigInt($5) }"
  _newForeignTempo :: Text -> Text -> Double -> Text -> Text -> ForeignTempo

toForeignTempo :: Tempo -> ForeignTempo
toForeignTempo t = _newForeignTempo freqNumerator freqDenominator time countNumerator countDenominator
  where
    freqNumerator = pack $ show $ numerator $ freq t
    freqDenominator = pack $ show $ denominator $ freq t
    time = realToFrac (utcTimeToPOSIXSeconds $ Estuary.Types.Tempo.time t)
    countNumerator = pack $ show $ numerator $ count t
    countDenominator = pack $ show $ denominator $ count t
          
toForeignTempoDebug :: Tempo -> IO ForeignTempo
toForeignTempoDebug t = do
  putStrLn "toForeignTempoDebug"
  -- let freqRatioInt = fromRational (freq t) :: Ratio Int
  -- putStrLn $ " freqRatioInt " ++ show freqRatioInt
  let freqNumerator = pack $ show $ numerator $ freq t
  putStrLn $ " freqNumerator " ++ show freqNumerator
  let freqDenominator = pack $ show $ denominator $ freq t
  putStrLn $ " freqDenominator " ++ show freqDenominator
  let time = realToFrac (utcTimeToPOSIXSeconds $ Estuary.Types.Tempo.time t)
  putStrLn $ " time " ++ show time
  -- let countRatioInt = fromRational (count t) :: Ratio Int
  -- putStrLn $ " countRatioInt " ++ show countRatioInt
  let countNumerator = pack $ show $ numerator $ count t 
  putStrLn $ " countNumerator " ++ show countNumerator
  let countDenominator = pack $ show $ denominator $ count t
  putStrLn $ " countDenominator " ++ show countDenominator
  pure $ _newForeignTempo freqNumerator freqDenominator time countNumerator countDenominator
    
foreign import javascript unsafe
  "$1.freqNumerator.toString() + \" \" + $1.freqDenominator.toString() + \" \" + $1.time.toString() + \" \" + $1.countNumerator.toString() + \" \" + $1.countDenominator.toString()"
  showForeignTempo :: ForeignTempo -> Text 
