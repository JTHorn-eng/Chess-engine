{- 
EVERYTHING MUST BE CREATED FROM FIRST PRINCIPLES

-}

module Algorithms (bubbleSort) where

deleteFromList :: (Eq a) => a -> [a] -> [a]
deleteFromList _ [] = []
deleteFromList b (l:ls)
    |  b == l = ls
    |  otherwise = l : deleteFromList b ls 

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (l:ls) = reverseList ls ++ [l] 


bubbleSort :: (Ord a, Eq a) => [a] -> [a]
bubbleSort [] = []
bubbleSort ls = (bubbleSort (deleteFromList (maximum ls) ls)) ++ [(maximum ls)]