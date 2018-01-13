module Tetris.UI where

import           Brick
import           Control.Concurrent
import           Control.Monad
import qualified Brick.BChan                as BC
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Graphics.Vty               as V
import           Data.Matrix
import           Tetris
import           Tetris.Field
import           Tetris.Piece
import           Tetris.Color

data Tick = Tick
data Name = Name deriving (Eq, Ord)

app :: App Field Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (attrName "Red", V.red `on` V.red)
    , (attrName "Blue", V.blue `on` V.blue)
    , (attrName "White", V.white `on` V.white)
    , (attrName "Yellow", V.yellow `on` V.yellow)
    , (attrName "Magenta", V.magenta `on` V.magenta)
    , (attrName "Cyan", V.cyan `on` V.cyan)
    , (attrName "Green", V.green `on` V.green)
    ]

drawUI :: Field -> [Widget Name]
drawUI f = [ C.center $ drawField f <+>
                        padRight (Pad 2) (drawNext f <=> padBottom (Pad 2) (drawPoints f)) ]

drawField :: Field -> Widget Name
drawField f = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "TETRIS")
    $ vBox row
    where
        fieldMat  = fieldMatrix f
        cutMatrix = submatrix 4 rows 1 cols $ fieldMat
        rows      = nrows $ fieldMatrix f
        cols      = ncols $ fieldMatrix f
        row       = [ hBox $ cells x | x <- [1..rows - 3]]
        cells r   = [ draw r x | x  <- [1..cols] ]
        draw r c  = withAttr (attrName . show $ getElem r c cutMatrix) block
        block     = str "  "

drawPoints :: Field -> Widget Name
drawPoints f = hLimit 11
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Points")
    $ C.hCenter $ padAll 1 $ str . show $ fieldPoints f

drawNext :: Field -> Widget Name
drawNext f = hLimit 11 $ vLimit 5
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Next")
    $ C.center
    $ vBox rows
    where
        piece       = fieldPieceType f !! 1
        pieceMatrix = fst3 $ initial piece
        allBlack r  = all (== Black) (getRow r pieceMatrix)
        rows        = [ hBox $ cells x | x <- [1..nrows pieceMatrix], not (allBlack x) ]
        cells r     = [ draw r x | x <- [1..ncols pieceMatrix] ]
        draw r c    = withAttr (attrName . show $ getElem r c pieceMatrix) block
        block       = str "  "

handleEvent :: Field -> BrickEvent Name Tick -> EventM Name (Next Field)
handleEvent f (AppEvent Tick)                       = critical $ shiftPieceDown f
handleEvent f (VtyEvent (V.EvKey V.KUp []))         = critical $ rotatePieceCCW f
handleEvent f (VtyEvent (V.EvKey V.KEnter []))      = critical $ dropPiece f
handleEvent f (VtyEvent (V.EvKey V.KDown []))       = critical $ shiftPieceDown f
handleEvent f (VtyEvent (V.EvKey V.KRight []))      = critical $ shiftPieceRight f
handleEvent f (VtyEvent (V.EvKey V.KLeft []))       = critical $ shiftPieceLeft f
handleEvent f (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt f
handleEvent f _                                     = continue f

{-| Check if game is lost -}
critical :: Either (Field, Fail) Field -> EventM Name (Next Field)
critical (Right f) = continue f
critical (Left (f, fail')) =
    case fail' of
        GameLost       -> halt f
        DropImpossible -> case checkRows f of
                              Right field -> critical $ placePiece field
                              Left (field,_) -> critical $ placePiece field
        _              -> continue f

begin :: Field -> Field
begin f =
    case placePiece f of
        Right x    -> x
        Left (x,_) -> x

main :: Int -> Int -> IO ()
main r c = do
    start      <- createField r c
    tick       <- BC.newBChan 10
    tickThread <- forkIO $ forever $ do
        threadDelay 500000
        BC.writeBChan tick Tick
    _ <- customMain (V.mkVty V.defaultConfig) (Just tick) app (begin start)
    killThread tickThread
