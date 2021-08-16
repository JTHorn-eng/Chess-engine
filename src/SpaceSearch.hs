module SpaceSearch where

import Board
import qualified Debug.Trace as D
import Toolkit

--for now the computer plays as black

dC = 5

--node, depth, alpha, beta, color
negamax :: Game -> Int -> Int -> Int -> Int -> Int
negamax game depth a b color
    | (depth == 0 && a >= b) = (heuristicScore game color)
    | otherwise = heuristicVal
    where
    heuristicVal :: Int
    heuristicVal = maximum (map lamb childNodes)
    lamb = \child -> findValue value child depth a b color 
    value = minBound :: Int
    childNodes = findAllStates (negate 1) game blackPieces
    blackPieces = (getPiecesByType (negate 1) (fst game))

--child node, depth, alpha, beta, color
findValue :: Int -> Game -> Int -> Int -> Int -> Int -> Int
findValue val child depth a b color =
    maximum (val, negate negMax)
    where
    negMax = negate (negamax child (depth - 1) (negate b) (negate alpha) (negate color))
    alpha = maximum (a, val)

findBestMove :: States -> Game
findBestMove states = fst $ Toolkit.maximumState tuples
    where
    tuples = zip states scores
    scores = map (\g -> negamax g dC (minBound :: Int) (minBound :: Int) (negate 1)) states