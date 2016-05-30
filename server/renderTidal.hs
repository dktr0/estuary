-- a brutally simple command-line utility to render a Tidal pattern as a WebDirt score
-- usage: renderTidal [cps] [number-of-cycles-to-render]
-- pattern is taken from first line of stdin
-- rendered output goes to stdout
-- if anything is wrong or missing, it will fail silently and output the message "error"

module Main where

import System.Environment
import TidalHint
import WebDirt

main = do
  [c,n] <- getArgs
  p <- getLine
  p' <- hintParamPattern p
  case p' of Left _ -> putStrLn "error"
             Right p'' -> putStrLn (show (render p'' (read c) (read n)))
