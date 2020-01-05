module Estuary.Languages.CineCer0.Types where

import Data.Time

import Estuary.Types.Tempo

 --              Tempo    Video Length      render T   eval T
type Signal a = Tempo -> NominalDiffTime -> UTCTime -> UTCTime -> a


constantSignal :: a -> Signal a 
constantSignal x = \_ _ _ _ -> x
