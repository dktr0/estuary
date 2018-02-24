module Main where

import Reflex.Dom
import Data.Time
import Estuary.WebDirt.WebDirt
import Estuary.WebDirt.SuperDirt
import Estuary.Protocol.Foreign
import Estuary.Types.Context

main :: IO ()
main = do
  now <- Data.Time.getCurrentTime
  wd <- webDirt
  sd <- superDirt
  protocol <- estuaryProtocol
  mainWidget $ estuaryWidget wd sd protocol (emptyContext now)
