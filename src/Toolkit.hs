
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
    in start ++ word ++ (reverse end)

insertAtIndex :: String -> String -> Int -> String
insertAtIndex word override atIndex =
    let
    start = take atIndex override
    end   = take (length override - (atIndex)) (reverse override)
    in start ++ word ++ (reverse end)     

removeFromList ::(Eq a) => [a] -> a -> [a]
removeFromList [] _ = []
removeFromList (x:xs) elem 
    | (x == elem) = [] ++ removeFromList xs elem
    | otherwise = [x] ++ removeFromList xs elem
  