module Types.SoundWidgetRequest where

import Control.Applicative
import Control.Monad
import Data.Map
import Types.Sound

data SoundWidgetRequest = CommonRequest (Map String String) |
                          ScrambleYourself |
                          Pulse (Map String String) |
                          BecomeSound Sound

instance Show SoundWidgetRequest where
  show (CommonRequest x) = "commonRequest" ++ (show x)
  show (ScrambleYourself) = "scramble"
  show (Pulse x) = "pulse" ++ (show x)
  show (BecomeSound x) = "become" ++ (show x)
