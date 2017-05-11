module Experiment where

-- data TransformedPattern = TransformedPattern PatternTransformer TransformedPattern | UntransformedPattern SpecificPattern | EmptyTransformedPattern deriving (Eq)

transformedPatternWidget :: MonadWidget t m => TransformedPattern -> m (Dynamic t (TransformedPattern, Event t (EditSignal a)))

transformedPatternWidget (EmptyTransformedPattern) = do
  x <- liftM (UntransformedPattern (S (Blank Inert))  <$) $ button "make me not empty"
  value <- holdDyn EmptyTransformedPattern x
  let event = (RebuildMe <$) x
  mapDyn (\y -> (y,event)) value

transformedPatternWidget (UntransformedPattern specificPattern) = do
  specPat <- specificContainer specificPattern never >>= mapDyn fst
  delete <- button "delete"
  let delete' = (EmptyTransformedPattern <$) delete
  let delete'' = (RebuildMe <$) delete -- RebuildMe would make just as much sense
  transform <- button "transform" >>= toggle False 

  transformer <- liftM (updated . joinDyn) $ flipppableWidget (parameteredPatternTransformer NoTransformer never) (return NoTransformer) False $ updated transform
  --^ an event, firing whenever the 'transformer' changes, containing the transformer

  let updatedValue = attachDynWith (\sp trans-> constDyn $ TransformedPattern trans sp) specPat transformer
  let updatedValue' = leftmost [updatedValue, fmap constDyn delete']
  unTransPat <- mapDyn UntransformedPattern specPat
  value <- liftM joinDyn $ holdDyn unTransPat updatedValue'
  --let transform' = (TransformedPattern Brak ...and the current value from x... <$) transform
  let rebuildEvents = leftmost [delete'', fmap (const RebuildMe) transformer]
  mapDyn (\x->(x,rebuildEvents)) value

  -- should be value of x with transforms added if necessary, and becomes EmptyTransformedPattern on delete
  -- basically, both delete and transform events change the value and issue rebuildme

transformedPatternWidget (TransformedPattern transformer pattern) = do
  x <- rebuildableWidget transformerWidget
  y <- rebuildableWidget transformedPatternWidget -- with pattern at first, then with whatever happens
  -- just like the untransformed pattern 