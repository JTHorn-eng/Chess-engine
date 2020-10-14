{-# LANGUAGE FlexibleContexts #-}

{- 
EVERYTHING MUST BE CREATED FROM FIRST PRINCIPLES

TODO 
-Sorting algorithms: merge, quick, counting, radix
-Graphs:Nodes, BFS, DFS, A*, Djikstra, Kruskal, Prims (use custom data type)
-Trees: basic operations, linear, binary, AVL, red,black (use custom data type)
-}
module Algorithms
(
    bubbleSort,
    insertionSort,
    mergeSort
)

where



deleteFromList :: (Eq a) => a -> [a] -> [a]
deleteFromList _ [] = []
deleteFromList b (l:ls)
    |  b == l = ls
    |  otherwise = l : deleteFromList b ls 

bubbleSort :: (Ord a, Eq a) => [a] -> [a]
bubbleSort [] = []
bubbleSort ls = (bubbleSort (deleteFromList (maximum ls) ls)) ++ [(maximum ls)]

insert :: (Num a, Ord a, Eq a) => [a] -> a -> [a]
insert [] x = [x]
insert (l:ls) x
    | (l >= x) = x:l:ls  
    | (ls == []) = l:x:[]
    | (l < x ) = l : insert ls x
    

insertion :: (Num a, Ord a, Eq a) => [a] -> [a] -> [a]
insertion ls [] = ls 
insertion ls (x:xs) = insertion (insert ls x) xs

insertionSort :: (Num a, Ord a, Eq a) => [a] -> [a]
insertionSort ls = insertion [] ls

half :: (Num a) => [a] -> ([a], [a])
half xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) (take (length xs) xs))

--thanks to https://www.programiz.com/dsa/merge-sort
merge :: (Eq a, Ord a, Num a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
--32, 6, 99, 5
merge (x:xs) (y:ys) 
    | (x < y) = x : (merge xs ([y] ++ ys))
    | (x >= y) = y : (merge ([x] ++ xs) ys)

split :: (Num a) => [[a]] -> [[a]]
split [] = []
split (x:xs) = (fst $ half x) : (snd $ half x) : (split xs)

recSplit ::(Num a) => [[a]] -> [[a]]
recSplit xss 
    | ((length (head xss )) > 0) = recSplit (split xss)
    | otherwise = xss

--test data [[1,8], [2, 7], [32, 6], [99, 5], [23,43], [12,567], [2,45], [23,24]]

recMerge :: (Eq a, Ord a, Num a) => [[a]] -> [[a]]
recMerge [] = []
recMerge (as:bs:xs) = (merge as bs) : recMerge xs
  
mergeSort' :: (Ord a, Num a) => [[a]] -> [[a]]
mergeSort' xss 
    | ((length xss) > 1) = mergeSort' (recMerge xss)
    | otherwise = xss

--test data [1,2,6,8,3,5,6,98,0,4,3,7,9] odd length

--test data [1,7,3,5,6,9,3,3,2,5,23] even length

mergeSort :: (Ord a, Num a) => [a] -> [a]
mergeSort xs = (mergeSort' (recSplit [xs])) !! 0








































































































































































































































































































































































































































































































































