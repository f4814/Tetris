module Tetris.Field
    ( Field(..)
    , Fail(..)
    , createField
    , emptyField
    ) where

import           Data.Matrix
import           Tetris.Piece
import           Tetris.Color
import qualified Data.Stream as S
import           System.Random

data Fail = RotationImpossible
          | DropImpossible
          | ShiftImpossible
          | GameLost
          deriving (Eq, Show)

data Field = Field {
      fieldMatrix           :: Matrix Color
    , fieldPieceType        :: S.Stream Piece
    , fieldPieceCoordinates :: [(Int, Int)]
    , fieldPieceCenterPoint :: (Float, Float)
    , fieldPoints           :: Integer
    } deriving (Eq, Show)

{-| Create a empty (all black) Field with 4 additional rows (b/c SRS)|-}
-- TODO: Random probably broken
createField :: Int -> Int -> IO Field
createField x y = do
    g <- getStdGen
    return $ Field (matrix (x+4) y (\_ -> Black)) (S.iterate (const (fst (random g))) I) [(-1,-1)] (-1,-1) 0

{-| Create a field with predefined pieces. Only for testing -}
emptyField :: Int -> Int -> Field
emptyField x y = Field (matrix (x+4) y (\_ -> Black)) (S.cycle [I,J,L,O,S,T,Z]) [(-1,-1)] (-1,-1) 0
