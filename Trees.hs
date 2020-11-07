module Trees(BinTree (..)) where


{-
    balancing trees, BTree

-}    

data BinTree a = E | N a (BinTree a) (BinTree a) deriving (Eq)

instance Functor BinTree where
    fmap f E = E
    fmap f (N a l r) = N (f a) (fmap f l) (fmap f r)

instance (Show a) => Show (BinTree a) where
    show E =  "--<Null>"
    show (N a l r) = "--<" ++ (show a) ++ ">\n     |\n     |" ++ (show l) ++ (show r)

insertBinTree :: (Ord a, Num a) => BinTree a -> a -> BinTree a
insertBinTree E x = N x E E
insertBinTree (N val l r) x
    | (val < x) = N val l (insertBinTree r x)
    | (val >= x) = N val (insertBinTree l x) r

searchBinTree :: (Eq a, Ord a, Num a) => BinTree a -> a -> Bool
searchBinTree E val = False
searchBinTree (N a l r) val
    | (val < a) = searchBinTree l val
    | (val == a) = True
    | (val > a) = searchBinTree r val

createBinTree :: (Ord a, Num a) => [a] -> BinTree a
createBinTree ls = foldl insertBinTree E ls

















