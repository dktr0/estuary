module Estuary.Tutorials.IntroTidalText where

import Reflex
import Reflex.Dom

import Data.IntMap.Strict
import Estuary.Tutorials.Tutorial
import Estuary.Types.View
import Estuary.Types.Definition
import Estuary.Types.Language
import Estuary.Tidal.Types

introTidalText::MonadWidget t m => Tutorial t m
introTidalText = Tutorial IntroTidalText (const $ return (constDyn empty, never))

{- |
v1 :: Language -> (View, Definition)
v1 English = (LabelView 0, LabelText "Welcome to the introductory tutorial to Tidalcycles (or MiniTidal)")
v1 a = (LabelView 0, LabelText $ translationDNE a)

v2 :: Language -> (View, Definition)
v2 English = (LabelView 1, LabelText "Click 'MidiTidal' to listen to the pattern below")
v2 a = (LabelView 1, LabelText $ translationDNE a)

v3:: Language -> (View, Definition)
v3 _ = (TidalTextView 2, EvaluableText (Edited "" "s \"bd bd\""))

p1= [v1,v2,v3]

v4 :: Language -> (View, Definition)
v4 English = (LabelView 0, LabelText "Welcome to the introductory tutorial to Tidalcycles (or MiniTidal)")
v4 a = (LabelView 0, LabelText $ translationDNE a)

v5 :: Language -> (View, Definition)
v5 English = (LabelView 1, LabelText "Click 'MidiTidal' to listen to the pattern below")
v5 a = (LabelView 1, LabelText $ translationDNE a)

v6:: Language -> (View, Definition)
v6 _ = (TidalTextView 2, EvaluableText $ Edited "" "s \"bd bd\" ")

p2 = [v4,v5,v6]

-- data Tutorial = Tutorial {
--   tutorialId::TutorialId,
--   pages:: Map Language [TutorialPage],
--   defaultLang::Language -- shown by default when a translation DNE to current lang
--   }
--
--   data Definition =
--     Structure TransformedPattern |
--     EvaluableText (Live String) |
--     -- EvaluableText String |
--     LabelText String
--     deriving (Eq,Show)
--
--   type DefinitionMap = Map.Map Int Definition
--
-- type TutorialPage = (View, DefinitionMap)

eng:: [TutorialPage]
eng = [ep1]



ep1::TutorialPage
ep1 = toTutorialPage [v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16] where
  v1 = (ViewDiv "tutorialTitle" . LabelView, LabelText "Welcome to the Introductory TidalCycles (or MiniTidal) tutorial!")
  v2 = (LabelView, LabelText "This tutorial will cover some of the basics of making music with TidalCycles. MiniTidal is a subset of TidalCycles that supports most typical Tidal operations (but not all), but everything shown here (and anything that works with MiniTidal) will also work with TidalCycles.")
  v3 = (LabelView, LabelText "Lets make some sound! Click 'eval' below. You should here a simple \"bassdrum clap\" drum pattern. (hit 'silence' to stop it)")
  v4 = (TidalTextView, EvaluableText (Edited "" "s \"bd cp\""))
  v5 = (LabelView, LabelText "In the text field above, we've specified a pattern of samples (specifically 'bd' and 'cp') that fill up a 'cycle' (which can be thought of sort of like musical bars if you like). Everything that appears within the quotes (\"\") divides a cycle into equal parts: the \"bd\" sample gets the first half of the cycle, and the \"cp\" gets the second half")
  v6 = (LabelView, LabelText "If we put a third element into our pattern we get a different rhythm:")
  v7 = (TidalTextView, EvaluableText (Edited "" "s \"bd cp hh\""))
  v8 = (LabelView, LabelText "Each sample gets 1/3rd of a cycle.")
  v9 = (LabelView, LabelText "A \"~\" placed in a Tidal pattern has a special designation as a 'rest'. So we can get rid of the 'hh' in the above example but preserve the rhythm:")
  v10 = (TidalTextView, EvaluableText (Edited "" "s \"bd cp ~\""))
  v11 = (LabelView, LabelText "Sometimes we want to subdivide a part of a cycle - for instance if we want 2 samples to play in the last half of a cycle instead of just one: ")
  v12 = (TidalTextView, EvaluableText (Edited "" "s \"bd [cp hh]\""))
  v13 = (LabelView, LabelText "Or if we want two samples to play at the same time we can enclose them in square brackets with a comma between them: ")
  v14 = (TidalTextView, EvaluableText (Edited "" "s \"bd [cp,hh]\""))
  v15 = (LabelView, LabelText "Both ideas together: ")
  v16 = (TidalTextView, EvaluableText (Edited "" "s \"bd [cp casio,hh]\""))

introTidalText :: Tutorial
-- introTidalText = Tutorial IntroTidalText (generateTutorial [p1,p2]) English
-- introTidalText = Tutorial IntroTidalText (singleton English [(LabelView 1,fromList [(1,LabelText "tesssst")])]) English
-- introTidalText = Tutorial IntroTidalText (singleton English [(TidalTextView 1,fromList [(1,EvaluableText (Edited "" "s \"bd cp\"") )])]) English
introTidalText = Tutorial IntroTidalText (singleton English eng) English

| -}
