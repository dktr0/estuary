module Recursive where

import Reflex
import Reflex.Dom
import Control.Monad
import Data.Map

data Two = One | Two deriving (Show)

twoWidget :: MonadWidget t m => Two -> m (Dynamic t Two)
twoWidget One = do
  button "One"
  return $ constDyn One
twoWidget Two = do
  button "Two"
  return $ constDyn Two

-- The widget above builds different widgets (although they are both buttons)
-- depending on its initial value. But there's no way of changing the value
-- that is reported, so...

twoWidget' :: MonadWidget t m => Two -> m (Dynamic t (Two,Event t Two))
twoWidget' One = do
  b <- button "One" >>= (Two <$)
  return $ constDyn (One,b)
twoWidget Two = do
  b <- button "Two" >>= (One <$)
  return $ constDyn (Two,b)

-- The widget above can signal that it wants to be something else but it can't
-- actually become that something else. So we make a container with widgetHold
-- that watches for change events and uses them to rebuild the contained widget

twoWidget'' :: MonadWidget t m => Two -> m (Dynamic t Two)
twoWidget'' i = mdo
  w <- liftM (joinDyn) $ widgetHold (twoWidget' i) events
  values <- mapDyn fst
  events <- liftM (switchPromptlyDyn) $ mapDyn snd w
  return values

main = mainWidget $ (twoWidget'' One) >>= display
