> {-# LANGUAGE RecursiveDo #-}
> module Growing where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Data.Functor.Misc -- For Const2
> import Multiple

2. then, widgets that can receive signals from their parents (i.e. "flash")

> data SimpleWidgetRequest = Set Simple | Flash

> requestableSimpleWidget :: (Ord k, MonadWidget t m) => k -> Simple -> Event t (SimpleWidgetRequest) -> m (Dynamic t Simple)
> requestableSimpleWidget key initialValue signal = do
>   let flashEvent = fforMaybe signal g
>   flashToggle <- toggle True flashEvent
>   attr <- forDyn flashToggle h
>   let buttons = forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   buttons' <- elDynAttr "div" attr buttons
>   let setEvent = fforMaybe signal f
>   value <- holdDyn initialValue (leftmost (buttons'++[setEvent]))
>   display value
>   return value
>   where f (Set x) = Just x
>         f _ = Nothing
>         g (Flash) = Just ()
>         g _ = Nothing
>         h True = singleton "style" "background-color: red; border: 3px solid black"
>         h False = singleton "style" "background-color: green; border: 3px solid black"
>
> main = mainWidget $ do
>  flash <- liftM (Flash <$) $ button "flash"
>  makeThree <- liftM (Set Three <$) $ button "makeThree"
>  let requests = leftmost [flash,makeThree]
>  widget1 <- requestableSimpleWidget 1 One requests
>  widget2 <- requestableSimpleWidget 2 Two requests
>  return ()
