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
  
-- if element is not in list y then add to output list
invCompareSets :: (Eq a) => [a] -> [a] -> [a]
invCompareSets [] _ = []
invCompareSets (x:xs) y 
    | (x `elem` y) = invCompareSets xs y
    | otherwise = [x] ++ invCompareSets xs y 

compareSets :: (Eq a) => [a] -> [a] -> [a]
compareSets [] _ = []
compareSets (x:xs) y 
    | (x `elem` y) = [x] ++ compareSets xs y
    | otherwise = compareSets xs y 

maximumState :: Ord a => [(t, a)] -> (t, a)
maximumState []      = error "maximum of empty list"
maximumState (x:xs)  = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | n < (snd p) = maxTail p ps
          | otherwise   = maxTail (m, n) ps

minimumState :: Ord a => [(t, a)] -> (t, a)
minimumState []     = error "minimum of empty list"
minimumState (x:xs) = minTail x xs
  where minTail currentMin [] = currentMin
        minTail (m, n) (p:ps)
          | n > (snd p) = minTail p ps
          | otherwise   = minTail (m, n) ps
