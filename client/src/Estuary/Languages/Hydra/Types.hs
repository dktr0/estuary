module Estuary.Languages.Hydra.Types where

  data Source =
    ConstantInt Int |
    ConstantDouble Double |
    List [Double] |
    Fast (Maybe Source) |
    --Speed Source | -- find a new place, it doesn't go here speed = 1.5
    --Bpm [Source] | -- audio
    Osc (Maybe Source) (Maybe Source) (Maybe Source) |
    Solid (Maybe Source) (Maybe Source) (Maybe Source) (Maybe Source) |
    Gradient (Maybe Source) |
    Noise (Maybe Source) (Maybe Source) |
    Shape (Maybe Source) (Maybe Source) (Maybe Source) |
    Voronoi (Maybe Source) (Maybe Source) (Maybe Source) |
    Scr Source |
    Brightness Source (Maybe Source) |
    Contrast Source (Maybe Source) |
    Color Source (Maybe Source) (Maybe Source) (Maybe Source) |
    Colorama Source (Maybe Source) |
    Invert Source (Maybe Source) |
    Luma Source (Maybe Source) (Maybe Source) |
    Posterize Source (Maybe Source) (Maybe Source) |
    Saturate Source (Maybe Source) |
    Shift Source (Maybe Source) (Maybe Source) (Maybe Source) (Maybe Source) |
    Thresh Source (Maybe Source) (Maybe Source) |
    Kaleid Source (Maybe Source) |
    Pixelate Source (Maybe Source) (Maybe Source) |
    Repeat Source (Maybe Source) (Maybe Source) (Maybe Source) (Maybe Source) |
    RepeatX Source (Maybe Source) (Maybe Source) |
    RepeatY Source (Maybe Source) (Maybe Source) |
    Rotate Source (Maybe Source) (Maybe Source) |
    Scale Source (Maybe Source) (Maybe Source) (Maybe Source) |
    Scroll Source (Maybe Source) (Maybe Source) (Maybe Source) (Maybe Source) |
    ScrollX Source (Maybe Source) (Maybe Source) |
    ScrollY Source (Maybe Source) (Maybe Source) |
    Add Source Source (Maybe Source) |
    Mult Source Source (Maybe Source) |
    Blend Source Source (Maybe Source) |
    Diff Source Source |
    Layer Source Source |
    Mask Source Source (Maybe Source) (Maybe Source) |
    Modulate Source (Maybe Source) |
    ModulateHue Source (Maybe Source) |
    ModulateKaleid Source (Maybe Source) |
    ModulatePixelate Source (Maybe Source) (Maybe Source) |
    ModulateRepeat Source (Maybe Source) (Maybe Source) (Maybe Source) (Maybe Source) |
    ModulateRepeatX Source (Maybe Source) (Maybe Source) |
    ModulateRepeatY Source (Maybe Source) (Maybe Source) |
    ModulateRotate Source (Maybe Source) (Maybe Source) |
    ModulateScale Source (Maybe Source) (Maybe Source) |
    ModulateScrollX Source (Maybe Source) (Maybe Source) |
    ModulateScrollY Source (Maybe Source) (Maybe Source)
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
