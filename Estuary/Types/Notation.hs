module Notation where

-- an L4 notation is one where changes take effect immediately
-- an L3 notation has a current state and a possible future state for evaluation

data Notation a = L4 a | L3 a a

instance Functor Notation where
  fmap f (L4 x) = L4 (f x)
  fmap f (L3 x y) = L3 (f x) y -- ? i.e. only apply to past evaluated states ?

instance Applicative Notation where
  pure x = L4 x
  f <*> a = (present f) (present a) --? i.e. only apply to past evaluated states?
  -- <*> :: Notation (a -> b) -> Notation a -> Notation b

instance Monad Notation where
  return x = L4 x
  (L4 x) >>= f = L4 (f x)
  (L3 x y) >>= f = L3 (f x) (f y)

data Three = One | Two | Three
data Six = Four | Five | Six
data Combined = Combined Three Six

data Liveness = L4 | L3

class Notation a where
  liveness :: a -> Liveness
  l4 :: a -> a
  l3 :: a -> a
  actual :: a -> a
  future :: a -> a
  eval :: a -> a
  edit :: a -> a -> a

data Liveness = L3 | L4
data Notation a = Notation Liveness a a
data Three = One | Two | Three
data Six = Four | Five | Six
data Combined = Combined Three Six

threeWidget :: m (Dynamic t (Notation Three))
sixWidget :: m (Dynamic t (Notation Six))
combinedWidget :: m (Dynamic t (Notation Combined))
combinedWidget = do
  e <- clickableDiv' "eval" $ eval
  a <- clickableDiv' "abandon" $ abandon
  l3 <- clickableDiv' "l3" $ l3
  l4 <- clickableDiv' "l4" $ l4
  three <- threeWidget
  six <- sixWidget

  Notation

  let edit = zipDynWith (\a b -> Combined a b) -- this makes sense
  let eval = zipDynWith (\a b -> Combined a b) -- ... but this is a puzzle
  -- it's like we need a binary function that behaves differently at different
  -- liveness levels

  foldDyn ($) i $ leftMost [e,a,l3,l4]

  x <- clickableDiv' "One" $ edit One
  y <- clickableDiv' "Two" $ edit Two
  z <- clickableDiv' "Three" $ edit Three
  pattern <- foldDyn ($) i $ leftMost [e,a,l3,l4,x,y,z]
  mapDyn (\a -> (a,never)) pattern




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

data TwoThrees = TwoThrees Three Three

-- but each of the child threes needs to have its own L3/L4 status...

-- this is like a kind of context that is preserved, so possibly a monad

data Notation TwoThrees = L4 (Notation Three) (Notation Three)

twoThreesNotationWidget :: MonadWidget t m => Notation TwoThrees -> Event t () -> m (Dynamic t (Notation TwoThrees, Event t GenericSignal))
twoThreesNotationWidget i _ = do
  e <- clickableDiv' "eval" $ eval
  a <- clickableDiv' "abandon" $ abandon
  l3 <- clickableDiv' "l3" $ l3
  l4 <- clickableDiv' "l4" $ l4
  x <- trivialNotationWidget (L3 One) never -- m (Dynamic t (Notation Three, Event t GenericSignal))
  y <- trivialNotationWidget (L3 One) never -- m (Dynamic t (Notation Three, Event t GenericSignal))
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
