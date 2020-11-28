-- Not finished!

import Prelude

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes _ 0 = [Empty]
hbalTreeNodes x n = help x n h
    
    
let (q, r) = (n - 1) `quotRem` 2
    in [Branch x left right| i <- [n-1, n-2 .. 0],
                             j <- [i-1 .. i+1],
                             j <= n-1,
                             j >= 0,
                             left <-  hbalTree x i,
                             right <- hbalTree x j]

