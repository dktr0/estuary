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
  --Src Source | -- screen/camera as input
  Brightness [Parameters] Source |
  Contrast [Parameters] Source |
  Colorama [Parameters] Source |
  Color [Parameters] Source |
  Invert [Parameters] Source |
  Luma [Parameters] Source |
  Posterize [Parameters] Source |
  Saturate [Parameters] Source |
  Shift [Parameters] Source |
  Thresh [Parameters] Source |
  Kaleid [Parameters] Source |
  Pixelate [Parameters] Source |
  Repeat [Parameters] Source |
  RepeatX [Parameters] Source |
  RepeatY [Parameters] Source |
  Rotate [Parameters] Source |
  Scale [Parameters] Source |
  Scroll [Parameters] Source |
  ScrollX [Parameters] Source |
  ScrollY [Parameters] Source |
  --modulators
  Modulate Source [Parameters] Source |
  ModulateHue Source [Parameters] Source |
  ModulateKaleid Source [Parameters] Source |
  ModulatePixelate Source [Parameters] Source |
  ModulateRepeat Source [Parameters] Source |
  ModulateRepeatX Source [Parameters] Source |
  ModulateRepeatY Source [Parameters] Source |
  ModulateRotate Source [Parameters] Source |
  ModulateScale Source [Parameters] Source |
  ModulateScrollX Source [Parameters] Source |
  ModulateScrollY Source [Parameters] Source |
  --operators
  Add Source [Parameters] Source |
  Mult Source [Parameters] Source |
  Blend Source [Parameters] Source |
  Diff Source Source |
  Layer Source Source |
  Mask Source [Parameters] Source
  --Speed Source | -- find a new place, it doesn't go here i.e. speed = 1.5
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
