module Tetris
    ( newPiece
    , placePiece
    , shiftPieceDown
    , shiftPieceUp
    , shiftPieceRight
    , shiftPieceLeft
    )where

import Data.Matrix
import System.Random
import Tetris.Piece
import Tetris.Color
import Tetris.Field

{-| Place a new Piece on the field |-}
newPiece :: Field -> IO (Either (Field, Fail) Field)
newPiece f = do
    p <- randomIO :: IO Piece
    return (placePiece p f)

{-| place a piece on the field. Only for testing purposes |-}
placePiece :: Piece -> Field -> Either (Field, Fail) Field
placePiece p f =
    if isLost f
        then Left  $ (f, GameLost)
        else Right $ Field (top <-> bottom) p (snd i) (fieldPoints f)
            where
                i = initial p
                old = fieldMatrix f
                top = extendTo Black 4 (ncols $ old) (fst i)
                bottom = submatrix 5 (nrows $ old) 1 (ncols $ old) old

{-| Check if game is lost |-}
isLost :: Field -> Bool
isLost f = any (/= Black) row
    where row = getRow 5 mat
          mat = fieldMatrix f

{-| Higher order shift function |-}
shiftPiece :: (Int -> Int) -> (Int -> Int) -> Field -> Either (Field, Fail) Field
shiftPiece rf cf f = if any (==True) $ map check l
                         then Left (f, ShiftImpossible)
                         else Right $ Field (modify m l)
                                            p
                                            (map (\(r,c) -> ((rf r), (cf c))) l)
                                            (fieldPoints f)
    where m = fieldMatrix f
          l = fieldPieceCoordinates f
          p = fieldPieceType f
          check (r,c) = getElem (rf r) (cf c) m /= Black
          modify mat [] = mat
          modify mat ((r,c):xs) = modify (setElem (color p) (r,c) mat) xs

{-| Shift piece down |-}
shiftPieceDown :: Field -> Either (Field, Fail) Field
shiftPieceDown = shiftPiece (subtract 1) id

{-| Shift piece left |-}
shiftPieceLeft :: Field -> Either (Field, Fail) Field
shiftPieceLeft = shiftPiece id (subtract 1)

{-| Shift piece right |-}
shiftPieceRight :: Field -> Either (Field, Fail) Field
shiftPieceRight = shiftPiece id (+1)

{-| Shift piece upwards. Only for testing purposes |-}
shiftPieceUp :: Field -> Either (Field, Fail) Field
shiftPieceUp = shiftPiece (+1) id

{-| Drop Piece to bottom |-}
dropPiece :: Field -> Either (Field, Fail) Field
dropPiece = undefined

{-| Rotate Piece couter-clockwise |-}
rotatePieceCCW :: Field -> Either (Field, Fail) Field
rotatePieceCCW = undefined

{-| Rotate Piece clockwise |-}
rotatePieceCW :: Field -> Either (Field, Fail) Field
rotatePieceCW = undefined
