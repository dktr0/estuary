module Notation where

data Liveness = L4 | L3

data Notation a = Notation a a Liveness


data Three = One | Two | Three
data Six = Four | Five | Six
data Combined = Combined Three Six

thinking :: m (Dynamic t (Notation Combined))
thinking = do
  x <- threeWidget -- m (Dynamic t (Notation Three))
  y <- sixWidget -- m (Dynamic t (Notation Six))
  return $ liftA2 (Combined) x y -- m (Dynamic t (Notation Combined))
  -- liftA2 (Combined) :: Notation Three -> Notation Six -> Notation Combined
  -- liftA2 (Combined) :: Dynamic (Notation Three) -> Dynamic (Notation Six) -> Dynamic (Notation Combined)
  -- liftA2 (Combined) x y :: Dynamic (Notation Combined)
  -- then return places this in m
  -- this is using the implementation of Applicative in Dynamic not in Notation though
  -- and it is not doing anything to track edit vs eval states, changes of liveness mode etc...
  -- note: I think liftA2 and zipDynWith are equivalent in this context?

data Combined = Combined (Notation Three) (Notation Six)

thinking :: m (Dynamic t Combined)
thinking = do
  x <- threeWidget -- m (Dynamic t (Notation Three))
  y <- sixWidget -- m (Dynamic t (Notation Six))
  let z = Combined <$> x <*> y
  -- Combined :: Notation Three -> Notation Six -> Combined
  -- Combined <$> :: Dynamic (Notation Three) -> Dynamic (Notation Six -> Combined)
  -- Combined <$> x :: Dynamic (Notation Six -> Combined)
  -- Combined <$> x <*> :: Dynamic (Notation Six) -> Dynamic Combined
  -- Combined <$> x <*> y :: Dynamic Combined
  return $ Combined <$> x <*> y
  -- now are preserving notation status of children
  -- but are not yet managing a notation status for the Combined type itself...

thinking2 :: m (Dynamic t (Notation Combined))
  x <- thinking -- m (Dynamic t Combined)

instance Notation' (Notation Combined) where
  past (Combined x y) =

data Combined = Combined Three Six

  


instance Functor Notation where
  fmap f (Notation x y l) = Notation (f x) (f y) l

instance Applicative Notation where
  pure x = Notation x x L4
  (Notation f g L4) <*> (Notation x y L4) =
  -- but this doesn't make sense we are always having to choose one liveness or another
  -- when we actually need to preserve liveness



  (L4 f) <*> (L4 x) = L4 (f x)
  (L4 f) <*> (L3 x y) =
  (L3 f g) <*> (L4 x) =
  (L3 f g) <*> (L3 x y) =

--



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
