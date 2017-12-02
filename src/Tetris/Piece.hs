module Tetris.Piece
    ( Piece(..)
    , initial
    , color
    ) where

import System.Random
import Data.Matrix
import Tetris.Color

data Piece = I | J | L | O | S | T | Z | Empty
    deriving (Show, Eq) -- TODO: Delete Show

color :: Piece -> Color
color x = case x of
              I -> Red
              J -> Blue
              L -> Orange
              O -> Yellow
              S -> Magenta
              T -> Cyan
              Z -> Green
              Empty -> Black


initial :: Piece -> (Matrix Color, [(Int, Int)])
initial x = case x of
                I -> (fromLists [[Black,Black,Black,Black],
                                [Red  ,Red  ,Red  ,Red  ],
                                [Black,Black,Black,Black],
                                [Black,Black,Black,Black]],
                                [(2,1), (2,2), (2,3), (2,4)])
                J -> (fromLists [[Blue,Black,Black],
                                [Blue,Blue,Blue],
                                [Black,Black,Black]],
                                [(1,1),(2,1),(2,2),(2,3)])
                L -> (fromLists [[Black,Black,Orange],
                                [Orange,Orange,Orange],
                                [Black,Black,Black]],
                                [(1,3),(2,1),(2,2),(2,3)])
                O -> (fromLists [[Black,Yellow,Yellow,Black],
                                [Black,Yellow,Yellow,Black],
                                [Black,Black,Black,Black]],
                                [(1,2),(1,3),(2,2),(3,2)])
                S -> (fromLists [[Black,Magenta,Magenta],
                                [Magenta,Magenta,Black],
                                [Black,Black,Black]],
                                [(1,2),(1,3),(2,1),(2,2)])
                T -> (fromLists [[Black,Cyan,Black],
                                [Cyan,Cyan,Cyan],
                                [Black,Black,Black]],
                                [(1,2),(2,1),(2,2),(2,3)])
                Z -> (fromLists [[Green,Green,Black],
                                [Black,Green,Green],
                                [Black,Black,Black]],
                                [(1,1),(1,2),(2,2),(2,3)])
                Empty -> (fromLists [[Black]], [(-1,-1)])

instance Random Piece where
    randomR (lo,hi) g = -- TODO: randomR completely broken
        case (fst rnd) `rem` 7 of
            0 -> (I, snd rnd)
            1 -> (J, snd rnd)
            2 -> (L, snd rnd)
            3 -> (O, snd rnd)
            4 -> (S, snd rnd)
            5 -> (T, snd rnd)
            6 -> (Z, snd rnd)
        where rnd = next g

    random = randomR (I,Z)
