module Estuary.Types.Color where

data Color =
  RGBA !Double !Double !Double !Double
  deriving (Eq)

instance Show Color where
  show (RGBA r g b a) = "rgba(" ++ show r ++ "%," ++ show g ++ "%," ++ show b ++ "%," ++ show a ++ "%" ++ ")"
