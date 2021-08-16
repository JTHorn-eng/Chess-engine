module SpaceSearch where

import Board
import qualified Debug.Trace as D

--for now the computer plays as black

dC = 5

--node, depth, alpha, beta, color
negamax :: Game -> Int -> Int -> Int -> Int -> Int
negamax game depth a b color
    | (depth == 0 && a >= b) = (nop (fst game) color)
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

