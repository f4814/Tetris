module Main where

import qualified Tetris.UI as UI
import           System.Environment

main :: IO ()
main = do
    r : c : [] <- map read <$> getArgs
    UI.main r c
