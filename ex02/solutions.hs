import Control.Exception (evaluate)
import Control.Monad (forM_)
import System.TimeIt (timeIt)

data Tree a = Node a [Tree a]
  deriving (Eq, Show, Read)

--------------- SOLUTIONS START HERE ----------------

-- Problem a 
leftish :: Int -> Tree Int
leftish 0 = Node 0 []
leftish n = Node n [(leftish (n-1)), (Node n [])]

-- Problem b
{-|
mirror :: Tree a -> Tree a
mirror (Node a children) = Node a (reverse children')
    where
        children' = mirrors children 
        mirrors :: [Tree a] -> [Tree a]
        mirrors [] = []
        mirrors (x:xs) = (mirror x):(mirrors xs)
-}

mirror :: Tree a -> Tree a
mirror (Node a children) = Node a (reverse (map (mirror) (children)))


-- Problem c
fringe_naive :: Tree a -> [a]
fringe_naive (Node v []) = [v]
fringe_naive (Node _ children) = run children
    where 
        run :: [Tree a] -> [a] 
        run [] = []
        run (y:ys) = (fringe_naive y) ++ (run ys)

-- O(n^2) complexity because we run '++' for every leaf 
-- '++' costs O(n)

-- Problem d
fringe :: Tree a -> [a]
fringe tree = reverse (walk [] tree)
    where
        walk :: [a] -> Tree a -> [a]
        walk acc (Node v []) = v:acc
        walk acc (Node v children) = walks acc children
            where
                walks :: [a] -> [Tree a] -> [a]
                walks acc [] = acc
                walks acc (x:xs) = walks acc' xs
                    where 
                        acc' = walk acc x

-- O(n) complexity
-- Notice that, at the end, the list is reversed before return.
-- This will be of interest for the next problem.


-- Problem e
listEquals :: Eq a => [a] -> [a] -> Bool
listEquals [] [] = True
listEquals (x:_) [] = False
listEquals [] (x:_) = False
listEquals (x:xs) (y:ys)
  | x == y = listEquals xs ys
  | otherwise = False

-- We construct (although not necessary) a function that checks
-- two lists for equality by comparing their heads each time and
-- then recursively moving on to their tails. If it finds that two
-- elements differ then it returns 'False' and stops further search.


same_fringe :: Eq a => Tree a -> Tree a -> Bool
same_fringe tree1 tree2 = listEquals (fringe tree1) (fringe tree2)

-- Since Haskell is lazy, the two fringe-lists will be constructed 
-- one element at a time, in order to be checked for equality. 
-- This is because the 'listEquals' function compares the lists 
-- head-to-head, recursively. 
-- Now, since 'fringe' reverses the list before return, the head of 
-- each returned list will be constructed as soon as 'reverse' finishes.
-- So, the complexity of 'same_fringe' is O(n), since the complexity
-- of 'fringe' is O(n).

-- Note: If we were to skip 'reverse' in 'fringe', we would be able
-- to see more clearly the results of Haskell's laziness. 
-- In that scenario, elements on each list would be constructed by 
-- 'fringe' at the same order that they are checked by 'listEquals'.
-- So, if a mismatch was encountered, then 'listEquals' would return
-- 'False' before 'fringe' constructed the whole lists.

-- Problem f

-- In an eager language, the 2 fringe lists would be constructed before
-- the equality check. 


-- Problem g
fibtree_naive :: Int -> Tree Int 
fibtree_naive 0 = Node 0 []
fibtree_naive 1 = Node 1 []
fibtree_naive n = Node (x1+x2) [Node x1 children1, Node x2 children2]
  where
    Node x1 children1 = fibtree_naive (n-1)
    Node x2 children2 = fibtree_naive (n-2)

-- It is easy to see, that space(T(n)) = space(T(n-1)) + space(T(n-2)) + 1
-- so, space(T(n)) >= 2*space(T(n-2)). In plain words, if 'n' grows by 2, 
-- then the Tree takes up more than TWICE the space. So, space(T(n)) grows 
-- exponentially in regards to 'n', and has O(2*n) as an upper bound. 


-- Problem h
fibtree :: Int -> Tree Int
fibtree 0 = Node 0 []
fibtree 1 = Node 1 [] 
fibtree 2 = Node 1 [Node 1 [], Node 0 []]
fibtree n = Node (x1+x2) [Node x1 ((Node x2 x2children):x1children), (Node x2 x2children)]
  where
    Node x1 ((Node x2 x2children):x1children) = fibtree (n-1)

-- T(n) = T(n-1) + T(n-2) = [T(n-2) + T(n-3)] + T(n-2) = 2*T(n-2) + T(n-3) ...
-- It is obvious that the naive approach would evaluate T(n-2) twice and so on.
-- In the function above, we use the same immutable "variable", (Node x2 x2children), for T(n-2). 
-- Otherwise, we would need to construct it twice, since T(n-2) is the right subtree of T(n) 
-- and the left subtree of T(n-1). Now, this way T(n-2) will be constructed and evaluated only once.
-- So, now the Fibonacci tree takes up O(n) space.

-- Problem i
-- 'mirror (fibtree n)' creates an altered version of each node of the tree by reversing the children 
-- list of the node and does the same for its children, recursively. As a result, it will take up space 
-- that is exponentially large, since it is applied seperately to subtrees that are the same.


t = Node 'a' [ Node 'b' [ Node 'd' [Node 'i' []]
                        , Node 'e' [Node 'j' [], Node 'k' []]
                        ]
             , Node 'c' [ Node 'f' [Node 'l' [], Node 'm' []]
                        , Node 'g' []
                        , Node 'h' [Node 'n' []]
                        ]
             ]

tm = Node 'a' [ Node 'c' [ Node 'h' [ Node 'n' []]
                         , Node 'g' []
                         , Node 'f' [Node 'm' [], Node 'l' []]
                         ]
              , Node 'b' [ Node 'e' [Node 'k' [], Node 'j' []]
                         , Node 'd' [Node 'i' []]
                         ]
              ]

test_correctness msg testcases = do
  putStr $ msg ++ ": " ++ (if and testcases then "OK" else "FAIL!!!") ++ "\n"

test_complexity msg range f = forM_ range $ \n -> do
  putStr $ msg ++ " with size " ++ show n ++ ", "
  timeIt $ evaluate $ f n

main = do
  test_correctness "mirror correctness" $
    [mirror t == tm] ++
    [mirror (mirror t) == t | n <- [0..100], let t = leftish n] ++
    [mirror (mirror t) == t | n <- [0..15], let t = fibtree n]
  test_correctness "fringe_naive correctness" $
    [fringe_naive t == "ijklmgn"] ++
    [fringe_naive (leftish n) == [0..n] | n <- [0..100]] ++
    [fringe_naive (mirror (leftish n)) == [n,n-1..0] | n <- [0..100]]
  test_complexity "fringe_naive leftish" [100, 1000, 10000, 20000, 30000] $
    length . fringe_naive . leftish
  test_correctness "fringe correctness" $
    [fringe t == "ijklmgn"] ++
    [fringe (leftish n) == [0..n] | n <- [0..100]] ++
    [fringe (mirror (leftish n)) == [n,n-1..0] | n <- [0..100]]
  test_complexity "fringe leftish" [100, 1000, 10000, 20000, 30000] $
    length . fringe . leftish
  test_correctness "same_fringe correctness" $
    [not (same_fringe (leftish n) (mirror (leftish n))) | n <- [1..100]] ++
    [not (same_fringe (leftish n) (mirror (leftish n))) | n <- [1..100]]
  test_complexity "mirror fibtree_naive" [20, 25, 30, 32] $ \n ->
    let t = fibtree_naive n in mirror (mirror t) == t
  test_complexity "mirror fibtree" [20, 25, 30, 32] $ \n ->
    let t = fibtree n in mirror (mirror t) == t
  test_complexity "same_fringe fibtree_naive" [20, 25, 30, 32] $ \n ->
    same_fringe (fibtree_naive n) (fibtree_naive (n+1))
  test_complexity "same_fringe fibtree" [20, 25, 30, 32, 34, 36] $ \n ->
    same_fringe (fibtree n) (fibtree (n+1))