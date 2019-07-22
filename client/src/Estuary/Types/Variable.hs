{-# LANGUAGE OverloadedStrings #-}
module Estuary.Types.Variable where

import Reflex

-- In this module we define the Variable type, which abstracts around a situation
-- that is very common across the Estuary project - wanting to represent values
-- that might be changed either by local edits or "other causes" (eg. network edits)
-- where we need to keep track of both local edits, and the value from all changes.

data Variable t a = Variable {
  currentValue :: Dynamic t a ,
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

-- not sure if we can define a monad instance for Variable but this is probably okay
-- (in many cases our values of this type are wrapped in another monadic context anyway)
