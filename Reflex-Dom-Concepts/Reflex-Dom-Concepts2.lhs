> {-# LANGUAGE RecursiveDo #-}
> module Tuple where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Data.Functor.Misc -- For Const2
> import Simple

Now we experiment with building widgets for more complex types built on top of
the Simple type, and using the simpleWidget definition. For example, a tuplet
composed of a pair of Simple values and edited with a pair of simpleWidgets.

> type Tuplet = (Simple,Simple)
>
> tupleWidget :: MonadWidget t m => Tuplet -> m (Dynamic t Tuplet)
> tupleWidget (i,j) = el "div" $ do
>   i' <- simpleWidget i
>   j' <- simpleWidget j
>   combineDyn (\x y -> "composite:" ++ show x ++ " " ++ (show y)) i' j' >>= display
>   combineDyn (,) i' j'
>
>   main = mainWidget $ tupleWidget (One, One) >>= display
