module Estuary.Types.Stroke where

import Estuary.Types.Color

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

data DashArray = DashArray Double Double deriving (Eq)

instance Show DashArray where
  show (DashArray x y) = show x ++ "," ++ show y

data Stroke = Stroke {
  strokeColor :: Color,
  strokeWidth :: Double,
  strokeLineCap :: LineCap,
  strokeLineJoin :: LineJoin,
  strokeDashArray :: DashArray
  } deriving (Show,Eq)
