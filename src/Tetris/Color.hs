module Tetris.Color
    (Color(..)
    ) where

data Color = Red | Blue | Orange | Yellow | Magenta | Cyan | Green | Black
    deriving (Eq)

instance Show Color where
    show x =
        case x of
            Red     -> "R"
            Blue    -> "Blue"
            Orange  -> "O"
            Yellow  -> "Y"
            Magenta -> "M"
            Cyan    -> "C"
            Green   -> "G"
            Black   -> "B"
