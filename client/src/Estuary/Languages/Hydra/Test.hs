{-# LANGUAGE OverloadedStrings #-}

module Estuary.Languages.Hydra.Test where

import Data.Text (Text)

test :: Int -> Text
test 0 = "2.0.out(O1)"
test 1 = "render()"
test 2 = "[2.0,3.0].out()"
test 3 = "[2.0,3.0,1.0,0.7].out()"
test 4 = ".fast(0.5).out(O2)"
test 5 = ".fast().out()"
test 6 = "osc(0.4,1.0,0.2).out()"
test 7 = "osc([0.4,0.5],1.0,0.2).out()"
test 8 = "osc().out()"
test 9 = "osc(10.0).out()"
test 10 = "osc(20,0.5).out()"
test 11 = "osc(1,2,3,4,5).out()" -- should be an error
test 12 = "osc().out()"
test _ = "invalid"


-- hydra (test #)
