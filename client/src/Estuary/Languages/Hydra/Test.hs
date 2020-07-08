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
test 10 = "gradient(osc()).out()"
test 11 = "noise().out()"
test 12 = "noise(0.5,0.7).out(O2)"
test 13 = "shape(osc(5,0.4)).out()"
test 14 = "shape(0.5,noise(),gradient()).out()"
test 15 = "voronoi().out(O1)"
test 16 = "voronoi([0.2,0.5]).out(O1)"
test 17 = "solid([-0.2,0.3,0.4].fast()).out()"
test 18 = "solid([0.2,0.3,0.4].fast(5)).out()"
test _ = "invalid"


-- hydra (test #)
