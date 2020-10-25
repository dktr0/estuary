{-# LANGUAGE OverloadedStrings #-}
module Estuary.Types.Variable where

import Reflex
import Reflex.Dom
import Control.Applicative
import Control.Monad

-- In this module we define the Variable type, which abstracts around a situation
-- that is very common across the Estuary project - wanting to represent values
-- that might be changed either by local edits or "other causes" (eg. network edits)
-- where we need to keep track of both local edits, and the value from all changes.

data Variable t a = Variable {
  currentValue :: Dynamic t a,
  localEdits :: Event t a
  }

instance Reflex t => Functor (Variable t) where
  fmap f (Variable d e) = Variable (fmap f d) (fmap f e)

instance Reflex t => Applicative (Variable t) where
  pure x = Variable (constDyn x) never
  (Variable fDyn fEdit) <*> (Variable xDyn xEdit) = Variable d e
    where
     d = fDyn <*> xDyn
     e = tagPromptlyDyn d $ leftmost [() <$ fEdit,() <$ xEdit]

instance Reflex t => Monad (Variable t) where
  (Variable aDyn aEvent) >>= f = Variable dynResult evResult
    where
      dynVar = fmap f aDyn -- :: Dynamic t (Variable t b)
      dynResult = join $ fmap currentValue dynVar -- :: Dynamic t b
      fEvent = switchPromptlyDyn $ fmap localEdits dynVar -- Event t b
      evResult = tagPromptlyDyn dynResult $ leftmost [() <$ aEvent, () <$ fEvent]

instance (Reflex t, Semigroup a) => Semigroup (Variable t a) where
  (Variable d1 e1) <> (Variable d2 e2) = Variable d e
    where
      d = d1 <> d2
      e = mergeWith (<>) [attachPromptlyDynWith (<>) d1 e2,attachPromptlyDynWith (flip (<>)) d2 e1]

instance (Reflex t, Monoid a) => Monoid (Variable t a) where
  mempty = Variable (constDyn mempty) never

returnVariable :: (Monad m, Reflex t, MonadSample t m, MonadHold t m) => Dynamic t a -> Event t a -> m (Variable t a)
returnVariable deltasDown editsUp = do
  i <- sample $ current deltasDown
  val <- holdDyn i $ leftmost [editsUp, updated deltasDown]
  return $ Variable val editsUp

-- the former reflexWidgetToEditor...
variableWidget :: MonadWidget t m => Dynamic t a -> (a -> Event t a -> m (Event t a)) -> m (Variable t a)
variableWidget delta widget = do
  i <- sample $ current delta
  x <- widget i $ updated delta
  returnVariable delta x

flattenDynamicVariable :: Reflex t => Dynamic t (Variable t a) -> Variable t a
flattenDynamicVariable x = Variable d e
  where
    d = join $ fmap currentValue x -- Dynamic (Dynamic t a)
    e = switchPromptlyDyn $ fmap localEdits x -- Dynamic (Event t a)
