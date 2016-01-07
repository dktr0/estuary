import Reflex
import Reflex.Dom

do newTweet <- el "div" $ do
     tweetBox <- textArea $
       def & attributes .~ constDyn ("maxlength" =: "140")
     tweetButton <- button "Tweet!"
     displayNumChars tweetBox
     return $ tag (current (value tweetBox)) tweetButton
   el "div" $ do
     latestTweet <- holdDyn "" newTweet
     text "Last status: "
     dynText latestTweet
