> {-# LANGUAGE RecursiveDo #-}
> module Multiple where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Data.Functor.Misc -- For Const2
> import Simple

What about a list containing a definite number of Simple types?

> type Multiple = [Simple]
>
> listWidget :: MonadWidget t m => Multiple -> m (Dynamic t Multiple)
> listWidget i = el "div" $ do
>   children <- forM i simpleWidget
>   value <- foldM (combineDyn (\ys y -> ys ++ [y])) (constDyn []) children
>   display value
>   return value
>
> main = mainWidget $ listWidget [One,Two,Three,Two,One] >>= display
