import Prelude

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree x n = [Branch x left right| i <- [n-1, n-2 .. 0],
                             j <- [i-1 .. i+1],
                             j <= n-1,
                             j >= 0,
                             left <-  hbalTree x i,
                             right <- hbalTree x j]

