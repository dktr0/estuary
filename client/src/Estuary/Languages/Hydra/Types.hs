module Estuary.Languages.Hydra.Types where

data Parameters =
  Parameters [Double] -- [0.2,0.4] -- [0.3,0.4,1.0]
  --Fast [Double] [Source]
  deriving (Show)

data Source =
  Osc [Parameters] | -- osc() -- osc(0.3) -- osc(0.3,0.5) -- osc(0,10,0.5) -- osc([0.4,0.5],1.0,0.2)
  Solid [Parameters] | -- solid() -- solid(0.5) -- solid(0.2,[0.1,0.2,0.3]) -- solid(1,0.5,1) -- solid(1,0.5,0.2,0.7)
  Gradient [Parameters] | -- gradient() -- gradient(0.4)
  Noise [Parameters] | -- noise() -- noise([5,10]) -- noise(0.5,0.7)
  Shape [Parameters] | -- shape() -- shape(2.0)
  Voronoi [Parameters] | -- voronoi() -- voronoi(10,0.5,0.1)
  --Speed Source | -- find a new place, it doesn't go here i.e. speed = 1.5
  --Bpm [Source] | -- audio
  --Src Source | -- screen/camera as input
  Brightness Source [Parameters] |
  Contrast Source [Parameters] |
  Color Source [Parameters] [Parameters] [Parameters] |
  Colorama Source [Parameters] |
  Invert Source [Parameters] |
  Luma Source [Parameters] [Parameters] |
  Posterize Source [Parameters] [Parameters] |
  Saturate Source [Parameters] |
  Shift Source [Parameters] [Parameters] [Parameters] [Parameters] |
  Thresh Source [Parameters] [Parameters] |
  Kaleid Source [Parameters] |
  Pixelate Source [Parameters] [Parameters] |
  Repeat Source [Parameters] [Parameters] [Parameters] [Parameters] |
  RepeatX Source [Parameters] [Parameters] |
  RepeatY Source [Parameters] [Parameters] |
  Rotate Source [Parameters] [Parameters] |
  Scale Source [Parameters] [Parameters] [Parameters] |
  Scroll Source [Parameters] [Parameters] [Parameters] [Parameters] |
  ScrollX Source [Parameters] [Parameters] |
  ScrollY Source [Parameters] [Parameters] |
  Add Source Source [Parameters] |
  Mult Source Source [Parameters] |
  Blend Source Source [Parameters] |
  Diff Source Source |
  Layer Source Source |
  Mask Source Source [Parameters] [Parameters] |
  Modulate Source [Parameters] |
  ModulateHue Source [Parameters] |
  ModulateKaleid Source [Parameters] |
  ModulatePixelate Source [Parameters] [Parameters] |
  ModulateRepeat Source [Parameters] [Parameters] [Parameters] [Parameters] |
  ModulateRepeatX Source [Parameters] [Parameters] |
  ModulateRepeatY Source [Parameters] [Parameters] |
  ModulateRotate Source [Parameters] [Parameters] |
  ModulateScale Source [Parameters] [Parameters] |
  ModulateScrollX Source [Parameters] [Parameters] |
  ModulateScrollY Source [Parameters] [Parameters]
  deriving (Show)

data Output =
  O0 |
  O1 |
  O2 |
  O3
  deriving (Show)


data Statement =
  Out Source Output |
  Render Output
  deriving (Show)
