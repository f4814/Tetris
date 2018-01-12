import Test.QuickCheck
import Tetris.Field
import Tetris.Color
import Tetris.Piece
import Tetris.UI.Logo
import Tetris

instance Arbitrary Field where
    arbitrary = do
        rows    <- choose (10,100)
        cols    <- choose (10,100)
        points  <- choose (1,100)
        row_mod <- choose (2, floor ((fromIntegral rows :: Float)/4) :: Int)
        col_mod <- choose (2, floor((fromIntegral cols :: Float)/4) :: Int)
        piece   <- choose (I,Z)
        let field = shiftPiece ((+)
                               . fromInteger $ fromIntegral row_mod) ((+)
                               . fromInteger $ fromIntegral col_mod) $
                        case placePiece_ piece (emptyField rows cols) of
                            Right x -> x
                            Left x  -> error "Couldn't create Field. This should not happen"
        return $ setFieldPoints points $ case field of
                                             Right x -> x
                                             Left x  -> error "Couldn't create Field. \
                                                              \ this should not happen"

setFieldPoints :: Integer -> Field -> Field
setFieldPoints p f = Field (fieldMatrix f) (fieldPieceType f) (fieldPieceCoordinates f) (fieldPieceCenterPoint f) p


reversableActionProp :: (Field -> Either (Field, Fail) Field)
                     -> (Field -> Either (Field, Fail) Field)
                     -> Field
                     -> Bool
reversableActionProp f f' field = case test of
                                      Right x -> True
                                      Left x -> False
    where test = do
                    f1 <- f field
                    f2 <- f' field
                    return ((f f2) == (f' f1))

testRotation :: (Field -> Either (Field, Fail) Field) -> Field -> Either (Field, Fail) Field
testRotation func field = do
    w <- func field
    x <- func w
    y <- func x
    z <- func y
    return z


main :: IO ()
main = do
    putStrLn ""
    putStrLn logo

    putStr "Up / Down shifting:     "
    quickCheckWith stdArgs { maxSuccess = 500 } (reversableActionProp shiftPieceUp shiftPieceDown)

    putStr "Left / Right shifting:  "
    quickCheckWith stdArgs { maxSuccess = 500 } (reversableActionProp shiftPieceLeft shiftPieceRight)

    -- TODO: Check error handling

    putStr "4x Rotation:            "
    quickCheckWith stdArgs { maxSuccess = 500 } (reversableActionProp (testRotation rotatePieceCCW) (testRotation rotatePieceCCW))
