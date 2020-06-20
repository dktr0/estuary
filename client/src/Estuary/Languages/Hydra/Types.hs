module Estuary.Languages.Hydra.Types where

  data Source =
    Constant Double |
    Multi [Double] |
    Fast [Double] |
    Speed [Source] |
    Bpm [Source] |
    Src Output |
    Osc Source [Source] [Source] [Source] |
    Solid Source [Source] [Source] [Source] [Source] |
    Gradient [Source] |
    Noise [Source] [Source] |
    Shape [Source] [Source] [Source] |
    Voronoi [Source] [Source] [Source] |
    Scr Source |
    Brightness Source [Source] |
    Contrast Source [Source] |
    Color [Source] [Source] [Source] |
    Colorama [Source] |
    Invert [Source] |
    Luma [Source] [Source] |
    Posterize [Source] [Source] |
    Saturate [Source] |
    Shift [Source] [Source] [Source] [Source] |
    Thresh [Source] [Source] |
    Kaleid Source |
    Pixelate [Source] [Source] |
    Repeat [Source] [Source] [Source] [Source] |
    RepeatX [Source] [Source] |
    RepeatY [Source] [Source] |
    Rotate [Source] [Source] |
    Scale [Source] [Source] [Source] |
    Scroll [Source] [Source] [Source] [Source] |
    ScrollX [Source] [Source] |
    ScrollY [Source] [Source] |
    Add Source Source [Source] |
    Mult Source Source [Source] |
    Blend Source Source [Source] |
    Diff Source Source |
    Layer Source Source |
    Mask Source Source [Source] [Source] |
    Modulate Source [Source] |
    ModulateHue Source [Source] |
    ModulateKaleid Source [Source] |
    ModulatePixelate Source [Source] [Source] |
    ModulateRepeat Source [Source] [Source] [Source] [Source] |
    ModulateRepeatX Source [Source] [Source] |
    ModulateRepeatY Source [Source] [Source] |
    ModulateRotate Source [Source] [Source] |
    ModulateScale Source [Source] [Source] |
    ModulateScrollX Source [Source] [Source] |
    ModulateScrollY Source [Source] [Source]
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
