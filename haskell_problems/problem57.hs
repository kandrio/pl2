import Prelude

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

add :: Ord a => a -> Tree a -> Tree a
add x Empty = Branch x Empty Empty
add x (Branch y left right)
    | z == LT = Branch y (add x left) right
    | z == GT = Branch y left (add x right)
    | otherwise = Branch y left right 
    where 
        z = x `compare` y

construct :: Ord a => [a] -> Tree a
construct [] = Empty
construct l = help l Empty
    where 
        help :: Ord a => [a] -> Tree a -> Tree a
        help [] tree = tree 
        help (x:xs) tree = help xs (add x tree)
          

