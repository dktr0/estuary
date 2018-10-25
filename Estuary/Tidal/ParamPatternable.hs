module Estuary.Tidal.ParamPatternable where

import qualified Sound.Tidal.Context as Tidal
import Estuary.Tidal.Types

-- | In order that the Estuary server is not dependent on Tidal, this module
-- is provided - for use in the Estuary client only - as a way of getting from
-- Estuary's Tidal-esque notations to an actual Tidal ParamPattern representation.

class ParamPatternable a where
  toParamPattern :: a -> Tidal.ParamPattern
  isEmptyFuture :: a -> Bool
  isEmptyPast :: a -> Bool

instance ParamPatternable SpecificPattern where
  toParamPattern (Accelerate x) = Tidal.accelerate $ Tidal.p $ show x
  toParamPattern (Bandf x) = Tidal.bandf $ Tidal.p $ show x
  toParamPattern (Bandq x) = Tidal.bandq $ Tidal.p $ show x
  toParamPattern (Begin x) = Tidal.begin $ Tidal.p $ show x
  toParamPattern (Coarse x) = Tidal.coarse $ Tidal.p $ show x
  toParamPattern (Crush x) = Tidal.crush $ Tidal.p $ show x
  toParamPattern (Cut x) = Tidal.cut $ Tidal.p $ show x
  toParamPattern (Cutoff x) = Tidal.cutoff $ Tidal.p $ show x
  toParamPattern (Delay x) = Tidal.delay $ Tidal.p $ show x
  toParamPattern (Delaytime x) = Tidal.delaytime $ Tidal.p $ show x
  toParamPattern (Delayfeedback x) = Tidal.delayfeedback $ Tidal.p $ show x
  toParamPattern (End x) = if isEmptyPast $ End x then Tidal.end $ Tidal.p "1" else Tidal.end $ Tidal.p $ show x
  toParamPattern (Gain x) = Tidal.gain $ Tidal.p $ show x
  toParamPattern (Hcutoff x) = Tidal.hcutoff $ Tidal.p $ show x
  toParamPattern (Hresonance x) = Tidal.hresonance $ Tidal.p $ show x
  toParamPattern (Loop x) = Tidal.loop $ Tidal.p $ show x
  toParamPattern (N x) = if isEmptyPast $ N x then Tidal.n $ Tidal.p "0" else Tidal.n $ Tidal.p $ show x
  toParamPattern (Pan x) = Tidal.pan $ Tidal.p $ show x
  toParamPattern (Resonance x) = Tidal.resonance $ Tidal.p $ show x
  toParamPattern (S x) = Tidal.s $ Tidal.p $ show x
  toParamPattern (Shape x) = Tidal.shape $ Tidal.p $ show x
  toParamPattern (Sound x) = Tidal.sound $ Tidal.p $ show x
  toParamPattern (Speed x) = Tidal.speed $ Tidal.p $ show x
  toParamPattern (Up x) = if isEmptyPast $ Up x then Tidal.up $ Tidal.p "0" else Tidal.up $ Tidal.p $ show x
  toParamPattern (Unit x) = Tidal.unit $ Tidal.p $ show x
  toParamPattern (Vowel x) = if isEmptyPast $ Vowel x then Tidal.vowel $ Tidal.p $ "t" else Tidal.vowel $ Tidal.p $ show x
  isEmptyPast (Accelerate x) = generalPatternIsEmptyPast x
  isEmptyPast (Bandf x) = generalPatternIsEmptyPast x
  isEmptyPast (Bandq x) = generalPatternIsEmptyPast x
  isEmptyPast (Begin x) = generalPatternIsEmptyPast x
  isEmptyPast (Coarse x) = generalPatternIsEmptyPast x
  isEmptyPast (Crush x) = generalPatternIsEmptyPast x
  isEmptyPast (Cut x) = generalPatternIsEmptyPast x
  isEmptyPast (Cutoff x) = generalPatternIsEmptyPast x
  isEmptyPast (Delay x) = generalPatternIsEmptyPast x
  isEmptyPast (Delaytime x) = generalPatternIsEmptyPast x
  isEmptyPast (Delayfeedback x) = generalPatternIsEmptyPast x
  isEmptyPast (End x) = generalPatternIsEmptyPast x
  isEmptyPast (Gain x) = generalPatternIsEmptyPast x
  isEmptyPast (Hcutoff x) = generalPatternIsEmptyPast x
  isEmptyPast (Hresonance x) = generalPatternIsEmptyPast x
  isEmptyPast (Loop x) = generalPatternIsEmptyPast x
  isEmptyPast (N x) = generalPatternIsEmptyPast x
  isEmptyPast (Pan x) = generalPatternIsEmptyPast x
  isEmptyPast (Resonance x) = generalPatternIsEmptyPast x
  isEmptyPast (S x) = generalPatternIsEmptyPast x
  isEmptyPast (Shape x) = generalPatternIsEmptyPast x
  isEmptyPast (Sound x) = generalPatternIsEmptyPast x
  isEmptyPast (Speed x) = generalPatternIsEmptyPast x
  isEmptyPast (Up x) = generalPatternIsEmptyPast x
  isEmptyPast (Unit x) = generalPatternIsEmptyPast x
  isEmptyPast (Vowel x) = generalPatternIsEmptyPast x
  isEmptyFuture (Accelerate x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Bandf x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Bandq x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Begin x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Coarse x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Crush x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Cut x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Cutoff x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Delay x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Delaytime x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Delayfeedback x) = generalPatternIsEmptyFuture x
  isEmptyFuture (End x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Gain x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Hcutoff x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Hresonance x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Loop x) = generalPatternIsEmptyFuture x
  isEmptyFuture (N x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Pan x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Resonance x) = generalPatternIsEmptyFuture x
  isEmptyFuture (S x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Shape x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Sound x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Speed x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Up x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Unit x) = generalPatternIsEmptyFuture x
  isEmptyFuture (Vowel x) = generalPatternIsEmptyFuture x

instance ParamPatternable TransformedPattern where
  toParamPattern (TransformedPattern (Combine sPat comb) EmptyTransformedPattern) = toParamPattern sPat
  toParamPattern (TransformedPattern t p) = applyPatternTransformer t (toParamPattern p)
  toParamPattern (UntransformedPattern u) = toParamPattern u
  toParamPattern (EmptyTransformedPattern) = Tidal.silence
  isEmptyFuture (UntransformedPattern u) = isEmptyFuture u
  isEmptyFuture (TransformedPattern t p) = isEmptyFuture p
  isEmptyFuture (EmptyTransformedPattern) = True
  isEmptyPast (TransformedPattern t p) = isEmptyPast p
  isEmptyPast (UntransformedPattern u) = isEmptyPast u
  isEmptyPast (EmptyTransformedPattern) = True

applyPatternTransformer :: PatternTransformer -> (Tidal.ParamPattern -> Tidal.ParamPattern)
applyPatternTransformer NoTransformer = id
applyPatternTransformer Rev = Tidal.rev
applyPatternTransformer (Slow f) = Tidal.slow $ pure f
applyPatternTransformer (Density f) = Tidal.density $ pure f
applyPatternTransformer Degrade = Tidal.degrade
applyPatternTransformer (DegradeBy d) = Tidal.degradeBy $ pure d
applyPatternTransformer (Every n t) = Tidal.every (pure n) (applyPatternTransformer t)
applyPatternTransformer (Brak) = Tidal.brak
applyPatternTransformer (Jux t) = Tidal.jux (applyPatternTransformer t)
applyPatternTransformer (Chop t) = Tidal.chop $ pure t
applyPatternTransformer (Combine p c) =  (toTidalCombinator c) $ toParamPattern p

toTidalCombinator :: PatternCombinator -> (Tidal.ParamPattern -> Tidal.ParamPattern -> Tidal.ParamPattern)
toTidalCombinator Merge = (Tidal.|=|)
toTidalCombinator Add = (Tidal.|+|)
toTidalCombinator Subtract = (Tidal.|-|)
toTidalCombinator Multiply = (Tidal.|*|)
toTidalCombinator Divide = (Tidal.|/|)

instance ParamPatternable StackedPatterns where
  toParamPattern (StackedPatterns xs) = Tidal.stack $ Prelude.map toParamPattern $ Prelude.filter (not . isEmptyPast) xs
  isEmptyPast (StackedPatterns xs) = and $ fmap isEmptyPast xs
  isEmptyFuture (StackedPatterns xs) = and $ fmap isEmptyFuture xs
