import Sound.Tidal.Context
import My.Cool.WebFramework

main = do
  s <- getStringFromBrowserWidgetWhenTheyPressEnter {- Matt -}
  let x = p s {- translate string into a Pattern}
  doSomethingCoolInBrowserWithPatternData x {- Matt -}
  maybeAlsoSendThePatternToTheServerButDavidIsWorkingOnThatNow s {- David -}
  ()
