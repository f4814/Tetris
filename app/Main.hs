module Main where

import qualified Tetris.UI as UI
import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    UI.main (read $ args !! 0) (read $ args !! 1)
