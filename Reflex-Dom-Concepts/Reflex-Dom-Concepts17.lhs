In this example, we navigate between multiple widgets, where each widget is able
to make changes to a shared global state (in this case, a Simple value in an MVar).

> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map
> import Control.Concurrent
> import Control.Monad.IO.Class

> data Simple = One | Two | Three deriving (Show,Eq,Ord,Read)

> main = do
>   mvar <- newMVar One
>   mainWidget $ do
>     multiPage One mvar
>     return ()

> multiPage :: MonadWidget t m => Simple -> MVar Simple -> m (Dynamic t Simple)
> multiPage var mvar = el "div" $ do
>   let pageMenu = constDyn $ fromList [(1::Int,"page 1"),(2,"page 2")]
>   pageDropDown <- dropdown 1 pageMenu def
>   let pageEvent = _dropdown_change pageDropDown
>   readvar <- performEvent $ fmap (liftIO . (const (readMVar mvar))) $ pageEvent
>   let pageDyn = current $ _dropdown_value pageDropDown
>   let pageAndVar = attach pageDyn readvar
>   let widgetChange = fmap (\(p,v) -> (page p) v mvar) pageAndVar
>   wh <- el "div" $ widgetHold (pageOne var mvar) widgetChange
>   return $ joinDyn wh
>   where page 1 = pageOne
>         page 2 = pageTwo

> pageOne :: MonadWidget t m => Simple -> MVar Simple -> m (Dynamic t Simple)
> pageOne var mvar = do
>   events <- liftM (leftmost) $ forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   value <- holdDyn var events
>   display value
>   performEvent $ fmap (liftIO . swapMVar mvar) $ events
>   return value

> pageTwo :: MonadWidget t m => Simple -> MVar Simple -> m (Dynamic t Simple)
> pageTwo var mvar = do
>   let menu = constDyn $ fromList [(One,"One"),(Two,"Two"),(Three,"Three")]
>   dd <- dropdown var menu def
>   let events = _dropdown_change dd
>   value <- holdDyn var events
>   display value
>   performEvent $ fmap (liftIO . swapMVar mvar) $ events
>   return value
