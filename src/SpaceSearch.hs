module SpaceSearch where

import qualified Board as B


type States = [B.Board]

{-
    iterate through pieces, find possible moves 
    and return these as a list of states ?

-}

{-
 - color: Black -1, White 1
 -
 -
 -}
generateMoves :: B.Board -> States
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
negamax :: B.Board -> Int -> Int -> Int -> Int -> Int
negamax node depth a b color
    | depth == 0 = color * findHeuristic node childNodes color
    | otherwise = negamaxRoutine minInt childNodes (newAlpha a newOptVal) b depth color
    where
    newOptVal = negamaxRoutine minInt childNodes a b depth color
    childNodes = generateMoves node
    minInt = minBound :: Int
    

findHeuristic ::  States -> Int -> Int
findHeuristic s c 
	| (c == negate 1 ) = minimum $ map boardScore s
	| otherwise = maximum $ map boardScore s
	


