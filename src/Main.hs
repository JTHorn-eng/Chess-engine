module Main where

import qualified SpaceSearch as Search
import Board


{- TODO

    -Implement display
    -Write heuristics for minimax
    -Write minimax
    -Optimisations
    -Testing
-}


main :: IO ()
main = do
    current <- currentBoard Board.init
    

currentBoard :: Board -> Board
currentBoard board = fst (Search.beginSearch DEPTH board)


DEPTH = 10

    
