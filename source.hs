import Reflex
import Reflex.Dom

main = mainWidget $ el "div" $ do
    newComposition <- el "div" $ do
            textBox <- textArea $ def & attributes .~ constDyn ("maxlength" =: "140")
            playButton <- button "PLAY!"
            return $  ffilter (/="") $ tag (current (value textBox)) playButton
    el "div" $ do
        compositionHistory <- foldDyn (:) [] newComposition
        text "Previous Compositions "
        display compositionHistory