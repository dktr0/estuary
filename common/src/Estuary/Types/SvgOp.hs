module Estuary.Types.SvgOp where

data Color =
  RGBA Double Double Double Double
  deriving (Eq)

instance Show Color where
  show (RGBA r g b a) = "rgba(" ++ show r ++ "%," ++ show g ++ "%," ++ show b ++ "%," ++ show (a/100) ++ ")"

data LineCap = Butt | Square | RoundCap deriving (Eq)

instance Show LineCap where
  show Butt = "butt"
  show Square = "square"
  show RoundCap = "round"

data LineJoin = Miter | RoundJoin | Bevel deriving (Eq)

instance Show LineJoin where
  show Miter = "miter"
  show RoundJoin = "round"
  show Bevel = "bevel"

data Stroke = Stroke {
  strokeColor :: Color,
  strokeWidth :: Double,
  strokeLineCap :: LineCap,
  strokeLineJoin :: LineJoin
  } deriving (Show,Eq)

data SvgOp =
  Line Double Double Double Double Stroke |
  Rect Double Double Double Double Stroke
  deriving (Show,Eq)
