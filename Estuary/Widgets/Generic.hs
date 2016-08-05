module Estuary.Widgets.Generic where

import Reflex
import Reflex.Dom
import Control.Monad

data GenericSignal = Ping | DeleteMe deriving (Eq, Show)

pingButton :: MonadWidget t m => String -> m (Dynamic t ((),Event t GenericSignal))
pingButton label = do
  x <- liftM (Ping <$) $ button label
  return $ constDyn ((),x)
