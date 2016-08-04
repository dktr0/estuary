module Estuary.Reflex.Utility where

import Reflex
import Reflex.Dom
import Control.Monad (liftM)

-- Anytime an event is received issue another event of a given constant value.

constEvent :: Reflex t => a -> Event t b -> Event t a
constEvent a b = fmap (const a) b

-- Whenever a received event matches a value, issue another event of a given
-- constant value.

matchEvent :: (Reflex t, Eq a) => a -> b -> Event t a -> Event t b
matchEvent a b = fmap (const  b) . ffilter (==a)

-- a button that, instead of producing Event t (), produces an event of
-- some constant value

button' :: (MonadWidget t m) => String -> a -> m (Event t a)
button' t r = do
  x <- button t
  return $ fmap (const r) x
