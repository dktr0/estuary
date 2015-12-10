import Sound.Tidal.Context

main = do
  d1 <- dirtStream
  d1 $ sound (p "bd cp")
  getLine
  
