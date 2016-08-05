module Estuary.Widgets.Generic where

import Reflex
import Reflex.Dom
import Control.Monad

data GenericSignal = Ping | DeleteMe deriving (Eq, Show)

plusButton:: MonadWidget t m => m (Dynamic t ((),Event t GenericSignal))
plusButton = do
  x <- liftM (Ping <$) $ button "+"
  return $ constDyn ((),x)
