{-# LANGUAGE OverloadedStrings #-}

module Estuary.Widgets.Config where

import Reflex hiding (Request,Response)
import Reflex.Dom hiding (Request,Response)

configWidget :: MonadWidget t m => m ()
configWidget = do
  text "Config stuff"
