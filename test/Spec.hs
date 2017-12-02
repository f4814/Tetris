import Test.QuickCheck
import Tetris.Field
import Tetris.Color

instance Arbitrary Field where
    arbitrary = do
        points  <- elements [1..]
        rows    <- elements [1..]
        columns <- elements [1..]
        piece   <- choose (I,Z)
        pieceCoordinates

reversableActionProp :: (Field -> Either (Field, Fail) Field)
                     -> (Field -> Either (Field, Fail) Field)
                     -> Field
                     -> Bool
reversableActionProp f f' field = undefined
    where test = do
                    f1 <- f field
                    f2 <- f' field
                    return ((f f2) == (f' f1))

main :: IO ()
main = do
    putStr "Up / Down shifting: "
    quickCheck (reversableActionProp shiftPieceUp shiftPieceDown)

    putStr "Left / Right shifting: "
    quickCheck (reversableActionProp shiftPieceLeft shiftPieceRight)
