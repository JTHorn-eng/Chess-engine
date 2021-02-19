module SpaceSearch where

import qualified Board as B


type States = [B.Board]

{-
    iterate through pieces, find possible moves 
    and return these as a list of states ?

 -}
generateMoves :: B.Board -> States
generateMoves board =

orderMoves :: States -> States
orderMoves states =



--explore child nodes 
--value, childNodes(states), alpha, beta, depth
negamaxRoutine :: Int -> States -> Int -> Int -> Int -> Int
negamaxRoutine val childNodes a b depth = 


--node, depth, alpha, beta, color
negamax :: B.Board -> Int -> Int -> Int -> Int -> Int
negamax node depth a b color
    | depth == 0 = color * (findHeuristic node)
    | otherwise = newOptVal
    where
    newOptVal = negamaxRoutine minInt childNodes a b depth
    childNodes = orderMoves $ generateMoves node
    minInt = minBound :: Int
    





