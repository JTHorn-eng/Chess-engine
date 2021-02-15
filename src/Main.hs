module Main where

{- TODO

    -Implement display
    -Write heuristics for minimax
    -Write minimax
    -Optimisations
    -Testing
-}

boardStatus :: String
boardStatus = "======="

engineResponse :: String -> String
engineResponse a = show a

runSim :: IO ()
runSim = do
    putStrLn $ boardStatus 
    putStrLn (">")
    input <- getLine
    if input /= "exit" then runSim else return ()
    putStrLn $ engineResponse input

main = do
    runSim



    
