import qualified Data.Char as C

module Toolkit where

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

-- only lowercase !
charToInt :: Char -> Int
charToInt val = (C.ord val) - 97