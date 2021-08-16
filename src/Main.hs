module Main where

import qualified SpaceSearch as Search
import Board
import Control.Monad as Control


{- TODO

    -Implement display
    -Write heuristics for minimax
    -Write minimax
    -Optimisations
    -Testin
-}

depth = 5

main :: IO ()
main = do
    game <- return initGame
    computerMove <- return $ Search.findBestMove [game]
    putStrLn computerMove
    --Board.displayStates [foundBoard]






    
