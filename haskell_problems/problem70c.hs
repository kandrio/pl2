import Prelude

data Tree a = Node a [Tree a] deriving (Show, Eq)

tree5 = Node 'a' [Node 'f' [Node 'g' []], Node 'c' [], Node 'b' [Node 'd' [], Node 'e' []]]

nnodes2 :: Tree a -> Int
nnodes2 (Node _ children) = 1 + (walk children) 
    where 
        walk :: [Tree a] -> Int
        walk [] = 0
        walk (x:xs) = (nnodes2 x) + (walk xs) 