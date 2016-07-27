module Main where

import Estuary.WebDirt.Stream
import Sound.Tidal.Context
import Reflex
import Reflex.Dom
import Control.Monad
import Control.Monad.IO.Class

main = do
  stream <- webDirtStream
  mainWidget $ el "div" $ do
    x <- textInput def
    performEvent_ $ fmap (liftIO . stream . sound . p) $ updated (_textInput_value x)
    return ()
