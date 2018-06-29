module Main where

import Reflex.Dom
import Data.Time
import Control.Concurrent.MVar

import Estuary.WebDirt.WebDirt
import Estuary.WebDirt.SuperDirt
import Estuary.Protocol.Foreign
import Estuary.Types.Context
import Estuary.Widgets.Estuary
import Estuary.WebDirt.SampleEngine

main :: IO ()
main = do
  now <- Data.Time.getCurrentTime
  let initialContext = emptyContext now
  wd <- webDirt
  sd <- superDirt
  protocol <- estuaryProtocol
  r <- newMVar $ render wd sd initialContext
  renderThread r
  mainWidget $ estuaryWidget r wd sd protocol initialContext
