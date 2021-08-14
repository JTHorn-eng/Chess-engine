module Main where

import qualified SpaceSearch as Search
import Board
import Control.Monad as Control


{- TODO

    -Implement display
    -Write heuristics for minimax
    -Write minimax
    -Optimisations
    -Testing
-}

depth = 5

main :: IO ()
main = do
    game <- return initGame

    foundBoard <- return (fst (Search.beginSearch depth game))
    Board.displayStates [foundBoard]





    
