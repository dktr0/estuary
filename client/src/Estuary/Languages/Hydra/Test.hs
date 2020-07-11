{-# LANGUAGE OverloadedStrings #-}

module Estuary.Languages.Hydra.Test where

import Data.Text (Text)

test :: Int -> Text
test 0 = "render()"
test 1 = "osc().out()"
test 2 = "osc(10).out()"
test 3 = "osc(20,0.5).out()"
test 4 = "osc(0.4,1.0,0.2).out()"
test 5 = "osc([0.4,0.5],1.0,0.2).out()"
test 6 = "osc(1,2,3,4,5).out()" -- last number shouldn't make anything
test 7 = "solid(0.5).out()"
test 8 = "solid(0.2,[0.1,0.2,0.3]).out(O3)"
test 9 = "gradient(0.4).out()"
test 10 = "noise().out()"
test 11 = "noise(0.5,0.7).out(O2)"
test 12 = "voronoi().out(O1)"
test 13 = "voronoi([5,0.8,0.3]).out()"
test 14 = "solid().brightness().out()"
test 15 = "solid().contrast([1.6,1.8,2]).out()"
test 16 = "osc().colorama().out()"
test 17 = "osc().brightness().colorama().out()"
test _ = "invalid"


-- hydra (test #)
