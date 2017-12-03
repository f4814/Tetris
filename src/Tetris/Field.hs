module Tetris.Field
    ( Field(..)
    , Fail(..)
    , createField
    ) where

import Data.Matrix
import Tetris.Piece
import Tetris.Color

data Fail = RotationImpossible
          | DropImpossible
          | ShiftImpossible
          | GameLost
          | Nothing
          deriving (Eq, Show)

data Field = Field {
      fieldMatrix           :: Matrix Color
    , fieldPieceType        :: Piece
    , fieldPieceCoordinates :: [(Int, Int)]
    , fieldPieceCenterPoint :: (Float, Float)
    , fieldPoints           :: Integer
    } deriving (Eq, Show)

{-| Create a empty (all black) Field with 4 additional rows (b/c SRS)|-}
createField :: Int -> Int -> Field
createField x y = Field (matrix (x+4) y (\_ -> Black)) Empty [(-1,-1)] (-1,-1) 0
