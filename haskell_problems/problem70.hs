import Prelude

data Tree a = Node a [Tree a] deriving (Show, Eq)

stringToTree :: [Char] -> Tree a
stringToTree l = walk 

walk :: [Char] -> [Char] -> Tree a
walk (x1:x2:xs) 
    | x1 \= '^' && x2 == '^' = (Node x1 []
    | otherwise = 

aaabcc^^^ab