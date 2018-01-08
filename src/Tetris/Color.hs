module Tetris.Color
    ( Color(..)
    ) where

data Color = Red | Blue | White | Yellow | Magenta | Cyan | Green | Black
    deriving (Eq)

instance Show Color where
    show x =
        case x of
            Red     -> "Red"
            Blue    -> "Blue"
            White  -> "White"
            Yellow  -> "Yellow"
            Magenta -> "Magenta"
            Cyan    -> "Cyan"
            Green   -> "Green"
            Black   -> "Black"
