import Prelude 

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

nnodes :: Tree a -> Int
nnodes Empty = 0
nnodes (Branch x child1 child2) = 
    1 + (nnodes child1) + (nnodes child2) 