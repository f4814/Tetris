module Tetris
    ( checkRows
    , dropPiece
    , isLost
    , placePiece
    , placePiece_
    , rotatePieceCCW
    , shiftPiece
    , shiftPieceDown
    , shiftPieceUp
    , shiftPieceRight
    , shiftPieceLeft
    ) where

import           Data.Matrix
import           Tetris.Piece
import           Tetris.Color
import           Tetris.Field

{-| Stuff for rows and columns -}
data RowsCols a = RowsCols { _rows :: a, _cols :: a }

{-| Place next Piece on the field -}
placePiece :: Field -> Either (Field, Fail) Field
placePiece f = placePiece_ piece (f { fieldPieceType = newType } )
    where
        newType = tail $ fieldPieceType f
        piece = head newType

{-| place a piece on the field. Only for testing purposes -}
placePiece_ :: Piece -> Field -> Either (Field, Fail) Field
placePiece_ p f =
    if isLost f
        then Left  $ (f, GameLost)
        else Right $ f { fieldMatrix = (top <-> bottom), fieldPieceCoordinates = (snd3 i),
                             fieldPieceCenterPoint = (thr3 i) }
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
shiftPiece :: RowsCols (Int -> Int) -> Field -> Either (Field, Fail) Field
shiftPiece (RowsCols {_rows=rf, _cols=cf}) f =
    if any (==True) $ map check l
        then let err = if (rf 0) > 0 then DropImpossible else ShiftImpossible
              in Left (f, err)
    else Right $ redraw (Field (withoutPiece l m)
                        (fieldPieceType f)
                        newL
                        (float rf . fst $ center, float cf . snd $ center)
                        (fieldPoints f))
          where m = fieldMatrix f  -- Matrix
                l = fieldPieceCoordinates f -- List
                center = fieldPieceCenterPoint f
                newL = map (\(r,c) -> (rf r, cf c)) l
                check (r,c) = safeGet (rf r) (cf c) (withoutPiece l m) /= Just Black -- Is shift possible

{-| Redraw current Piece -}
redraw :: Field -> Field
redraw f = f { fieldMatrix = newMat }
    where
        m = fieldMatrix f
        c = fieldPieceCoordinates f
        newColor = color . head . fieldPieceType $ f
        newMat = set newColor c m

{-| Integer functions on Floats. Needed for the Center Point |-}
float :: (Int -> Int) -> Float -> Float
float f' x = diff + (fromIntegral . f' . floor $ x)
    where diff = x - (fromIntegral . floor $ x)

{-| Delete the current piece from the Matrix |-}
withoutPiece :: [(Int, Int)] -> Matrix Color -> Matrix Color
withoutPiece [] mat = mat
withoutPiece ((r,c):xs) mat = withoutPiece xs (setElem Black (r,c) mat)

{-| Set a list of Coordinates in a Matrix |-}
set :: a -> [(Int, Int)] -> Matrix a -> Matrix a
set _ [] m = m
set e (y:ys) m = set e ys (setElem e y m)

{-| Shift piece down |-}
shiftPieceDown :: Field -> Either (Field, Fail) Field
shiftPieceDown = shiftPiece (RowsCols {_rows = (+1), _cols = id})

{-| Shift piece left |-}
shiftPieceLeft :: Field -> Either (Field, Fail) Field
shiftPieceLeft = shiftPiece (RowsCols {_rows = id, _cols = (subtract 1) })

{-| Shift piece right |-}
shiftPieceRight :: Field -> Either (Field, Fail) Field
shiftPieceRight = shiftPiece (RowsCols { _rows = id, _cols = (+1) })

{-| Shift piece upwards. Only for testing purposes |-}
shiftPieceUp :: Field -> Either (Field, Fail) Field
shiftPieceUp = shiftPiece (RowsCols { _rows = (subtract 1), _cols = id })

{-| Drop Piece to bottom |-}
dropPiece :: Field -> Either (Field, Fail) Field
dropPiece f = do
    f' <- shiftPieceDown f
    dropPiece f'


{-| Check for full rows and delete them.
    WARNING: The coordinates of the current piece are NOT modified.
             Hence `placePiece` should be called after this function
             deletes somethhing
-}
checkRows :: Field -> Either (Field, Fail) Field
checkRows f = if length rows /= 0
                  then Right $ f { fieldMatrix = deleteRows rows m, fieldPoints = (fieldPoints f) + fromIntegral (length rows * 100) }
                  else Left $ (f, NothingDeleted)
    where
        m                     = fieldMatrix f
        rows                  = [ x | x <- [1..nrows m], all (/= Black) (getRow x m) ]
        deleteRows [] mat     = mat
        deleteRows (r:rs) mat = let extended = matrix (nrows m) 1 black <|> (deleteRows rs mat)
                                 in (matrix 1 (ncols m) black) <-> minorMatrix r 1 extended
        black (_,_)           = Black


-- r2 = (c1 + pr - pc)
-- c2 = (pr + pc - r1)
{-| Rotate Piece couter-clockwise |-}
rotatePieceCCW :: Field -> Either (Field, Fail) Field
rotatePieceCCW f = if any (check . toInt . newCord) l
                       then Left $ (f, RotationImpossible)
                       else Right $ f { fieldMatrix = (set (color . head $ p) coordinates w),
                                        fieldPieceCoordinates = coordinates }
    where m = fieldMatrix f -- Matrix
          p = fieldPieceType f -- Piece
          l = fieldPieceCoordinates f -- List
          c = fieldPieceCenterPoint f -- Center
          w = withoutPiece l m
          coordinates     = map (toInt . newCord) l
          newCord (rc,cc) = (fromIntegral cc + fst c - snd c, fst c + snd c - fromIntegral rc)
          check (rm,cm)   = safeGet rm cm w /= Just Black
          toInt (x,y)     = (round x, round y)
