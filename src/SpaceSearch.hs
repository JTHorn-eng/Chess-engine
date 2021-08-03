module SpaceSearch where

import Board

{-
type States = [Board]

generateMoves :: Board -> States
generateMoves board = foldl (\p -> B.checkMove p board) [] board  
 
newAlpha :: Int -> Int -> Int
newAlpha a newVal = maximum (a, newVal)

--explore child nodes 
--value, childNodes(states), alpha, beta, depth
--output tuple = (value, new alpha)
negamaxRoutine :: Int -> States -> Int -> Int -> Int -> Int -> Int
negamaxRoutine val childNodes a b depth color = newVal
    where
    newVal = maximum out
    out = map (\n -> negate (negamax n (depth - 1) (negate b) (negate a) (negate color))) childNodes
    

--node, depth, alpha, beta, color
negamax :: Board -> Int -> Int -> Int -> Int -> Int
negamax node depth a b color
    | depth == 0 = color * findHeuristic node childNodes color
    | otherwise = negamaxRoutine minInt childNodes (newAlpha a newOptVal) b depth color
    where
    newOptVal = negamaxRoutine minInt childNodes a b depth color
    childNodes = generateMoves node
    minInt = minBound :: Int
-}

--node, depth, alpha, beta, color
negamax :: Board -> Int -> Int -> Int -> Int -> Int
negamax node depth a b color
    | (depth == 0) = color * (snd foundNode)
    | otherwise = negamax (fst foundNode) (depth - 1) (negate b) (negate a) (negate color)
    where
    minInt = minBound :: Int
    foundNode = (Board.findHeuristic node color)
   
beginSearch :: Int
beginSearch = negamax (Board.init) (2) (negate 100000) (10000000) (negate 1)