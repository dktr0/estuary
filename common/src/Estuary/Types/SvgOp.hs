module Estuary.Types.SvgOp where

data SvgOp =
  Line Double Double Double Double |
  Rect Double Double Double Double
  deriving (Show,Eq)
