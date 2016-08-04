> {-# LANGUAGE RecursiveDo #-}
> module Main where
> import Reflex
> import Reflex.Dom
> import Control.Monad
> import Data.Map as M
> import Data.Either
> import Data.Functor.Misc -- For Const2
> import qualified Data.Maybe

> import Estuary.Reflex.Container

> data Simple = One | Two | Three deriving (Show,Eq)
> type Multiple = [Simple]
> data SimpleWidgetRequest = Set Simple | Flash
> data WidgetEvent = DeleteMe | MakeSimple deriving (Show,Eq)
> data Misc = Add deriving (Show,Eq)

> requestableSimpleWidget :: MonadWidget t m => Simple -> Event t (SimpleWidgetRequest) -> m (Dynamic t (Simple, Event t WidgetEvent))
> requestableSimpleWidget initialValue signal = do
>   let flashEvent = fforMaybe signal g
>   flashToggle <- toggle True flashEvent
>   attr <- forDyn flashToggle h
>   let buttons = forM [One,Two,Three] (\x -> liftM (x <$) (button (show x)))
>   buttons' <- elDynAttr "div" attr buttons
>   deleteButton <- liftM (DeleteMe <$) $ button "-"
>   let setEvent = fforMaybe signal f
>   value <- holdDyn initialValue (leftmost (buttons'++[setEvent]))
>   display value
>   forDyn value (\a-> (a,deleteButton))
>   where f (Set x) = Just x
>         f _ = Nothing
>         g (Flash) = Just ()
>         g _ = Nothing
>         h True = singleton "style" "background-color: red; border: 3px solid black"
>         h False = singleton "style" "background-color: green; border: 3px solid black"


> miscWidget:: MonadWidget t m => m (Dynamic t (Misc,Event t WidgetEvent))
> miscWidget = el "div" $ do
>   x <- liftM (MakeSimple <$) $ button "  +  "
>   return $ constDyn (Add,x)


> builder :: MonadWidget t m => Either Simple Misc -> Event t SimpleWidgetRequest
>   -> m (Dynamic t (Either Simple Misc,Event t WidgetEvent))
> builder (Left simp) e = do
>   x <- requestableSimpleWidget simp e
>   mapDyn (\(a,b) -> (Left a,b)) x
> builder (Right misc) _ = do
>   x <- miscWidget
>   mapDyn (\(a,b) -> (Right a,b)) x


> growAndShrinkWidget :: MonadWidget t m => m (Dynamic t [Simple])
> growAndShrinkWidget = el "div" $ mdo
>   let initialMap = fromList [(0::Int,Right Add)]
>   let cEvents = mergeWith union [deleteMap,makeSimpleMap]
>   let rEvents = never
>   (values,events) <- container initialMap cEvents rEvents builder
>   let deleteKeys = fmap (keys . M.filter (==DeleteMe)) events
>   let deleteList = fmap (concat . Prelude.map (\k -> [(k,Delete),(k+1,Delete)])) deleteKeys
>   let deleteMap = fmap (fromList) deleteList
>   let makeSimpleKeys = fmap (keys . M.filter (==MakeSimple)) events
>   let makeSimpleList = fmap (concat . Prelude.map (\k -> [(k,Insert (Right Add)),(k+1,Insert (Left One))])) makeSimpleKeys
>   let makeSimpleMap = fmap (fromList) makeSimpleList
>   mapDyn (elems . mapMaybe (id) . fmap (either (Just) (const Nothing))) values

> main = mainWidget $ growAndShrinkWidget >>= display
