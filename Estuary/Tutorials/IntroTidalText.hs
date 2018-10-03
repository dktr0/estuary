module Estuary.Tutorials.IntroTidalText where

import Estuary.Tutorials.Tutorial
import Estuary.Types.View
import Estuary.Types.Definition
import Estuary.Types.Language

v1 :: Language -> (View, Definition)
v1 English = (LabelView 0, LabelText "Welcome to the introductory tutorial to Tidalcycles (or MiniTidal)")
v1 a = (LabelView 0, LabelText $ translationDNE a)

v2 :: Language -> (View, Definition)
v2 English = (LabelView 1, LabelText "Click 'MidiTidal' to listen to the pattern below")
v2 a = (LabelView 1, LabelText $ translationDNE a)

v3:: Language -> (View, Definition)
v3 _ = (TidalTextView 2, EvaluableText "s \"bd bd\" ")


p1= [v1,v2,v3]


v4 :: Language -> (View, Definition)
v4 English = (LabelView 0, LabelText "Welcome to the introductory tutorial to Tidalcycles (or MiniTidal)")
v4 a = (LabelView 0, LabelText $ translationDNE a)

v5 :: Language -> (View, Definition)
v5 English = (LabelView 1, LabelText "Click 'MidiTidal' to listen to the pattern below")
v5 a = (LabelView 1, LabelText $ translationDNE a)

v6:: Language -> (View, Definition)
v6 _ = (TidalTextView 2, EvaluableText "s \"bd bd\" ")

p2 = [v4,v5,v6]


introTidalText :: Tutorial
introTidalText = Tutorial IntroTidalText (generateTutorial [p1,p2])
