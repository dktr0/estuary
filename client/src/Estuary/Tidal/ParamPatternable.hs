module Estuary.Tidal.ParamPatternable where

import qualified Sound.Tidal.Context as Tidal
import Estuary.Tidal.Types

-- | In order that the Estuary server is not dependent on Tidal, this module
-- is provided - for use in the Estuary client only - as a way of getting from
-- Estuary's Tidal-esque notations to an actual Tidal ParamPattern representation.

class ParamPatternable a where
  toParamPattern :: a -> Tidal.ControlPattern
  isEmptyFuture :: a -> Bool
  isEmptyPast :: a -> Bool

-- Tidal's main pattern parser comes in two flavours currently,
-- a flavour that returns an Either ParseError a, and a flavour that
-- throws exceptions. When using this parser in the service of our
-- structure editor we want neither of this - just silent failure.
parseBP' :: (Tidal.Enumerable a, Tidal.Parseable a) => String -> Tidal.Pattern a
parseBP' = (either (const Tidal.silence)  id). Tidal.parseBP 

instance ParamPatternable SpecificPattern where
  toParamPattern (Accelerate x) = Tidal.accelerate $ parseBP' $ show x
  toParamPattern (Bandf x) = Tidal.bandf $ parseBP' $ show x
  toParamPattern (Bandq x) = Tidal.bandq $ parseBP' $ show x
  toParamPattern (Begin x) = Tidal.begin $ parseBP' $ show x
  toParamPattern (Coarse x) = Tidal.coarse $ parseBP' $ show x
  toParamPattern (Crush x) = Tidal.crush $ parseBP' $ show x
  toParamPattern (Cut x) = Tidal.cut $ parseBP' $ show x
  toParamPattern (Cutoff x) = Tidal.cutoff $ parseBP' $ show x
  toParamPattern (Delay x) = Tidal.delay $ parseBP' $ show x
  toParamPattern (Delaytime x) = Tidal.delaytime $ parseBP' $ show x
  toParamPattern (Delayfeedback x) = Tidal.delayfeedback $ parseBP' $ show x
  toParamPattern (End x) = if isEmptyPast $ End x then Tidal.end $ parseBP' "1" else Tidal.end $ parseBP' $ show x
  toParamPattern (Gain x) = Tidal.gain $ parseBP' $ show x
  toParamPattern (Hcutoff x) = Tidal.hcutoff $ parseBP' $ show x
  toParamPattern (Hresonance x) = Tidal.hresonance $ parseBP' $ show x
  toParamPattern (Loop x) = Tidal.loop $ parseBP' $ show x
  toParamPattern (N x) = if isEmptyPast $ N x then Tidal.n $ parseBP' "0" else Tidal.n $ parseBP' $ show x
  toParamPattern (Pan x) = Tidal.pan $ parseBP' $ show x
  toParamPattern (Resonance x) = Tidal.resonance $ parseBP' $ show x
  toParamPattern (S x) = Tidal.s $ parseBP' $ show x
  toParamPattern (Shape x) = Tidal.shape $ parseBP' $ show x
  toParamPattern (Sound x) = Tidal.sound $ parseBP' $ show x
  toParamPattern (Speed x) = Tidal.speed $ parseBP' $ show x
  toParamPattern (Up x) = if isEmptyPast $ Up x then Tidal.up $ parseBP' "0" else Tidal.up $ parseBP' $ show x
  toParamPattern (Unit x) = Tidal.unit $ parseBP' $ show x
  toParamPattern (Vowel x) = if isEmptyPast $ Vowel x then Tidal.vowel $ parseBP' $ "t" else Tidal.vowel $ parseBP' $ show x
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

applyPatternTransformer :: PatternTransformer -> (Tidal.ControlPattern -> Tidal.ControlPattern)
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

toTidalCombinator :: PatternCombinator -> (Tidal.ControlPattern -> Tidal.ControlPattern -> Tidal.ControlPattern)
toTidalCombinator Merge = (Tidal.#)
toTidalCombinator Add = (Tidal.|+|)
toTidalCombinator Subtract = (Tidal.|-|)
toTidalCombinator Multiply = (Tidal.|*|)
toTidalCombinator Divide = (Tidal.|/|)

instance ParamPatternable StackedPatterns where
  toParamPattern (StackedPatterns xs) = Tidal.stack $ Prelude.map toParamPattern $ Prelude.filter (not . isEmptyPast) xs
  isEmptyPast (StackedPatterns xs) = and $ fmap isEmptyPast xs
  isEmptyFuture (StackedPatterns xs) = and $ fmap isEmptyFuture xs
