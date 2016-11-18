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

isL3 :: Notation a -> Bool
isL3 (L3 _ _) = True
isL3 (L4 _) = False

isL4 :: Notation a -> Bool
isL4 (L3 _ _) = False
isL4 (L4 _) = True


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

-- what happens when a Notation a type includes a child type Notation b?

data Four = Four | Other Three

fourNotationWidget :: MonadWidget t m => Notation Four -> Event t () -> m (Dynamic t (Notation Four, Event t GenericSignal))
fourNotationWidget i _ = do
  e <- clickableDiv' "eval" $ eval
  a <- clickableDiv' "abandon" $ abandon
  l3 <- clickableDiv' "l3" $ l3
  l4 <- clickableDiv' "l4" $ l4
  x <- clickableDiv' "Four" $ edit Four
  y <- trivialNotationWidget (L3 One) never
  yNotation <- fmap fst y
  ySignal <- fmap snd y
  let l4events = ffilter (isL4) $ updated yNotation
  let l3events = ffilter (isL3) $ updated yNotation
  -- when L4 changes this widget becomes Other and the...

  -- ...if the L3 status of child widgets isn't reported all the way up the chain
  -- then it will not be possible to have the overall document state used as
  -- a basic collaboration mechanism in distributed settings!

data Four = Four | Other (Notation Three)   -- ???

-- we need to be able to have a data structure something like this

L4 (Group [L3 One Two,L3 Two One,L4 Three,L3 Three Three])
