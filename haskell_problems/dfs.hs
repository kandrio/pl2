import Prelude

data Tree a = Node a [Tree a] deriving (Show, Eq)

dfs :: Tree a -> Tree (a, Int)
dfs tree = fst (walk tree 0)
    where
        walk :: Tree a -> Int -> (Tree (a, Int), Int)
        walk (Node v c) n = (Node (v, n+1) children, n')
            where 
                (children, n') = walks c (n+1)
                walks :: [Tree a] -> Int -> ([Tree (a, Int)], Int)
                walks [] n = ([], n)
                walks (y:ys) n = ((z:zs), n'')
                    where
                        (z, n') = walk y n
                        (zs, n'') = walks ys n'
                                



