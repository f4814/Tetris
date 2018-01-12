module Tetris.Piece
    ( Piece(..)
    , initial
    , color
    , snd3
    , fst3
    , thr3
    ) where

import System.Random
import Data.Matrix
import Tetris.Color

data Piece = I | J | L | O | S | T | Z
    deriving (Eq)

color :: Piece -> Color
color x = case x of
              I -> Red
              J -> Blue
              L -> White
              O -> Yellow
              S -> Magenta
              T -> Cyan
              Z -> Green


initial :: Piece -> (Matrix Color, [(Int, Int)], (Float,Float))
initial x = case x of
                I -> (fromLists [[Black,Black,Black,Black],
                                [Red  ,Red  ,Red  ,Red  ],
                                [Black,Black,Black,Black],
                                [Black,Black,Black,Black]],
                                [(2,1), (2,2), (2,3), (2,4)],
                                (2.5,2.5))
                J -> (fromLists [[Blue,Black,Black],
                                [Blue,Blue,Blue],
                                [Black,Black,Black]],
                                [(1,1),(2,1),(2,2),(2,3)],
                                (2,2))
                L -> (fromLists [[Black,Black,White],
                                [White,White,White],
                                [Black,Black,Black]],
                                [(1,3),(2,1),(2,2),(2,3)],
                                (2,2))
                O -> (fromLists [[Black,Yellow,Yellow,Black],
                                [Black,Yellow,Yellow,Black],
                                [Black,Black,Black,Black]],
                                [(1,2),(1,3),(2,2),(3,2)],
                                (2.5,2.5))
                S -> (fromLists [[Black,Magenta,Magenta],
                                [Magenta,Magenta,Black],
                                [Black,Black,Black]],
                                [(1,2),(1,3),(2,1),(2,2)],
                                (2,2))
                T -> (fromLists [[Black,Cyan,Black],
                                [Cyan,Cyan,Cyan],
                                [Black,Black,Black]],
                                [(1,2),(2,1),(2,2),(2,3)],
                                (2,2))
                Z -> (fromLists [[Green,Green,Black],
                                [Black,Green,Green],
                                [Black,Black,Black]],
                                [(1,1),(1,2),(2,2),(2,3)],
                                (2,2))

-- RandomR is broken, but it works
instance Random Piece where
    randomR (_,_) g =
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

fst3 :: (a, b, c) -> a
fst3 (x,_,_) = x

snd3 :: (a, b, c) -> b
snd3 (_,y,_) = y

thr3 :: (a, b, c) -> c
thr3 (_,_,z) = z
