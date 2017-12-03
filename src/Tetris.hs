module Tetris
    ( newPiece
    , placePiece
    , shiftPiece
    , shiftPieceDown
    , shiftPieceUp
    , shiftPieceRight
    , shiftPieceLeft
    , rotatePieceCCW
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
-- TODO: Field center point wrong
placePiece :: Piece -> Field -> Either (Field, Fail) Field
placePiece p f =
    if isLost f
        then Left  $ (f, GameLost)
        else Right $ Field (top <-> bottom) p (snd3 i) (thr3 i) (fieldPoints f)
            where
                i = initial p
                old = fieldMatrix f
                top = extendTo Black 4 (ncols $ old) (fst3 i)
                bottom = submatrix 5 (nrows $ old) 1 (ncols $ old) old

{-| Check if game is lost |-}
isLost :: Field -> Bool
isLost f = any (/= Black) row
    where row = getRow 5 mat
          mat = fieldMatrix f

{-| Higher order shift function |-}
-- TODO: Check matrix length
shiftPiece :: (Int -> Int) -> (Int -> Int) -> Field -> Either (Field, Fail) Field
shiftPiece rf cf f = if any (==True) $ map check l
                         then Left (Field (withoutPiece l m) Empty [(0,0)] center s, ShiftImpossible)
                         else Right $ Field (modify m l)
                                            p
                                            (map (\(r,c) -> ((rf r), (cf c))) l)
                                            (float rf . fst $ center,
                                             float cf . snd $ center)
                                            (fieldPoints f)
          where m = fieldMatrix f  -- Matrix
                l = fieldPieceCoordinates f -- List
                p = fieldPieceType f -- Piece
                s = fieldPoints f -- Score
                center = fieldPieceCenterPoint f -- Center
                check (r,c) = getElem (rf r) (cf c) (withoutPiece l m) /= Black -- Is shift possible
                modify mat [] = mat -- Move Piece
                modify mat ((r,c):xs) = modify (setElem (color p) (rf r,cf c) . withoutPiece l $ mat) xs

{-| Integer functions on Floats. Needed for the Center Point |-}
float :: (Int -> Int) -> Float -> Float
float f' x = diff + (fromIntegral . f' . floor $ x)
    where diff = x - (fromIntegral . floor $ x)

{-| Delete the current piece from the Matrix |-}
withoutPiece :: [(Int, Int)] -> Matrix Color -> Matrix Color
withoutPiece [] mat = mat -- Delete moving piece
withoutPiece ((r,c):xs) mat = withoutPiece xs (setElem Black (r,c) mat)

{-| Set a list of Coordinates in a Matrix |-}
set :: a -> [(Int, Int)] -> Matrix a -> Matrix a
set _ [] m = m
set e (y:ys) m = set e ys (setElem e y m)

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

-- r2 = (c1 + pr - pc)
-- c2 = (pr + pc - r1)
{-| Rotate Piece couter-clockwise |-}
rotatePieceCCW :: Field -> Either (Field, Fail) Field
rotatePieceCCW f = if any (check . toInt . newCord) l
                       then Left $ (Field m p l c (fieldPoints f), RotationImpossible)
                       else Right $ Field (set (color p) coordinates w)
                                          p coordinates c (fieldPoints f)
    where m = fieldMatrix f -- Matrix
          p = fieldPieceType f -- Piece
          l = fieldPieceCoordinates $ f -- List
          c = fieldPieceCenterPoint f -- Center
          w = withoutPiece l m
          coordinates = map (toInt . newCord) l
          newCord (rc,cc) = (fromIntegral cc + fst c - snd c, fst c + snd c - fromIntegral rc)
          check (rm,cm) = getElem rm cm w /= Black
          toInt (x,y) = (round x, round y)
