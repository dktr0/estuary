module Estuary.Types.Transform where

data Rotate =  Rotate Double  deriving (Eq)
data Scale = Scale Double deriving (Eq)
data Translate =  Translate Double Double  deriving (Eq)
data Skew = Skew Double deriving (Eq)

instance Show Rotate where
  show (Rotate x) = "rotate(" ++ show x ++ "deg" ++ ") "

instance Show Scale where
  show (Scale x) = "scale(" ++ show x ++  ") "

instance Show Skew where
  show (Skew x) = "skew(" ++ show x ++ "deg" ++ ") "

instance Show Translate where
  show (Translate x y) = "translate(" ++ show x ++ "%" ++ "," ++ show y ++ "%" ++ ") "

data Transform = Transform {
  tRotate :: Rotate,
  tScale :: Scale,
  tSkew :: Skew,
  tTranslate :: Translate
} deriving (Show,Eq)
