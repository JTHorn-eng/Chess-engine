
module Toolkit where


import qualified Data.Char as C


safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

-- only lowercase !
charToInt :: Char -> Int
charToInt val = (C.ord val) - 97

replaceAtIndex :: String -> String -> Int -> String
replaceAtIndex word override atIndex = 
    let 
    start = take atIndex override
    end   = take (length override - (atIndex + 2)) (reverse override)
    in start ++ word ++ end
        