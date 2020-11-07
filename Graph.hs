module Graph (Graph (..), graphFind, graphInsert) where

{- TODO
   Create instance for show
    

-}

--each node is either empty or has a value with a list of pointers to other values
data Graph a = Empty | Node a [a] (Graph a) deriving (Show, Eq)   

--for testing graphs use:
-- let graph = (Node 1 [1,2,3] (Node 4 [2,7,3] (Node 9 [2,4,6] (Empty))))



graphFind :: (Eq a) => Graph a -> a -> Bool
graphFind (Empty) _ = False
graphFind (Node a ls rest) b = if (a == b) then True else graphFind rest b

graphInsert :: Graph a -> a -> [a] -> Graph a
graphInsert Empty x xs = Node x xs (Empty)
graphInsert (Node a ls Empty) x xs = Node a ls (Node x xs (Empty))
graphInsert (Node a ls rest) x xs = Node a ls (graphInsert rest x xs)  

makeGraph :: [(a, [a])] -> Graph a
makeGraph [] = Empty
makeGraph (l:ls) = graphInsert (makeGraph ls) (fst l) (snd l) 





