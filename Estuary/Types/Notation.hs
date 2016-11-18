module Notation where

-- an L4 notation is one where changes take effect immediately
-- an L3 notation has a current state and a possible future state for evaluation

data Notation a = L4 a | L3 a a

present :: Notation a -> a
present (L4 x) = x
present (L3 x _) = x

future :: Notation a -> a
future (L4 x) = x
future (L3 _ x) = x

edit :: a ->  
edit x (L3 y _) = L3 y x
edit x (L4 _) = L4 x

eval :: Notation a -> Notation a
eval (L3 _ x) = L3 x x
eval x = x

abandon :: Notation a -> Notation a
abandon (L3 x _) = L3 x x
abandon x = x

l4 :: Notation a -> Notation a
l4 (L3 _ x) = L4 x
l4 x = x

l3 :: Notation a -> Notation a
l3 (L4 x) = L3 x x
l3 x = x


-- Examples:

data Three = One | Two | Three

trivialNotationWidget :: MonadWidget t m => Notation Three -> Event t () -> m (Dynamic t (Notation Three, Event t GenericSignal))
trivialNotationWidget i _ = do
  e <- clickableDiv' "eval" $ eval
  a <- clickableDiv' "abandon" $ abandon
  l3 <- clickableDiv' "l3" $ l3
  l4 <- clickableDiv' "l4" $ l4
  x <- clickableDiv' "One" $ edit One
  y <- clickableDiv' "Two" $ edit Two
  z <- clickableDiv' "Three" $ edit Three
  pattern <- foldDyn ($) i $ leftMost [e,a,l3,l4,x,y,z]
  mapDyn (\a -> (a,never)) pattern
