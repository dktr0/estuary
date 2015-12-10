import Sound.Tidal.Context

{- compile with: ghc compiledTidal.hs -o compiledTidal -}
{- run with ./compiledTidal -}

setupTidalStream :: IO (OscPattern -> IO ())
setupTidalStream = do
  putStrLn "setting up Tidal"
  (cps, getNow) <- bpsUtils
  d1 <- dirtStream
  return d1

playSimplePattern :: (OscPattern -> t) -> String -> t
playSimplePattern s x = s $ sound (p x)

playAndWait s x = do
  playSimplePattern s x
  getLine

main :: IO ()
main = do
  d1 <- setupTidalStream
  let ps = ["bd","bd cp","[bd bd] cp","[bd*4] [cp ~ arpy*2 bd]"]
  putStrLn "press enter to advance pattern until finished"
  getLine
  mapM (playAndWait d1) ps
  putStrLn "finished"
