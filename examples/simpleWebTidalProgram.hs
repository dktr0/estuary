import Sound.Tidal.Context
import My.Cool.WebFramework

playPattern stream pattern = stream $ sound (p pattern)

main = do
  d1 <- dirtStream {- but this will be on server side only -}
  s <- getStringFromBrowserWidgetWhenTheyPressEnter
  playPattern d1 s {- playPattern gets done when s "fires" -} 
  ()
