module SpaceSearch where

import Board
import qualified Debug.Trace as D

--node, depth, alpha, beta, color
negamax :: Game -> Int -> Int -> Int -> Int -> (Game, Int)
negamax game depth a b color
    | (depth == 0) = (fst foundNode, snd foundNode * color)
    | otherwise = negamax (fst foundNode) (depth - 1) (negate b) (negate a) (negate color)
    where
    minInt = minBound :: Int
    foundNode = Board.findHeuristic game color
   
beginSearch :: Int -> Game -> (Game, Int)
beginSearch depth game = negamax game depth (negate 100000) 10000000 (negate 1)

