module Tetris.Field
    ( Field(..)
    , Fail(..)
    , createField
    , emptyField
    ) where

import           Data.Matrix
import           Tetris.Piece
import           Tetris.Color
import           System.Random

data Fail = RotationImpossible
          | DropImpossible
          | ShiftImpossible
          | NothingDeleted
          | GameLost
          deriving (Eq, Show)

data Field = Field {
      fieldMatrix           :: Matrix Color
    , fieldPieceType        :: [Piece]
    , fieldPieceCoordinates :: [(Int, Int)]
    , fieldPieceCenterPoint :: (Float, Float)
    , fieldPoints           :: Integer
    } deriving (Eq)

instance Show Field where
    show f = show m ++ "\n" ++ show c ++ "\n" ++ show e ++ "\n" ++ show p
        where
            m = fieldMatrix f
            c = fieldPieceCoordinates f
            e = fieldPieceCenterPoint f
            p = fieldPoints f

{-| Create a empty (all black) Field with 4 additional rows on top (b/c SRS)|-}
createField :: Int -> Int -> IO Field
createField x y = do
    g <- newStdGen
    return $ Field (matrix (x+4) y (\_ -> Black)) (stream g) [(-1,-1)] (-1,-1) 0

stream :: RandomGen g => g -> [Piece]
stream g = randoms g

{-| Create a field with predefined pieces. Only for testing -}
emptyField :: Int -> Int -> Field
emptyField x y = Field (matrix (x+4) y (\_ -> Black)) (cycle [I,J,L,O,S,T,Z]) [(-1,-1)] (-1,-1) 0
