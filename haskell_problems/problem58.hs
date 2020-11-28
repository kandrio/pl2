import Prelude 

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

mirror2 :: Tree a -> Tree b -> Bool
mirror2 Empty Empty = True
mirror2 (Branch _ left1 right1) (Branch _ left2 right2) = 
    (mirror2 left1 right2) && (mirror2 right1 left2)
mirror2 _ _ = False

mirror :: Tree a -> Bool
mirror Empty = True 
mirror (Branch _ left right) = mirror2 left right

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n - 1) `quotRem` 2
    in [Branch 'x' left right| i <- [q .. q+r],
                               left <-  cbalTree i,
                               right <- cbalTree (n-i-1)]

symCbalTree :: Int -> [Tree Char]
symCbalTree n = [tree | tree <- cbalTree n, mirror tree]
    