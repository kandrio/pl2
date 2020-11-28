import Prelude
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isSpace)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import GHC.Conc (setAllocationCounter, enableAllocationLimit, getAllocationCounter)
import GHC.Stats

data Tree a = Node a [Tree a] deriving (Show, Eq)

appendChild :: [Int] -> [Int] -> [Int]
appendChild (child:_) children = child:children 

addChild :: Map Int [Int] -> Int -> Int -> Map Int [Int]
addChild mymap father child
    | (Map.notMember father mymap) == True = Map.insert father [child] mymap
    | otherwise = Map.insertWith (appendChild) father [child] mymap

constructMap :: [Int] -> Int -> Map Int Int -> Map Int [Int] -> (Map Int Int, Map Int [Int])
constructMap [] _ mapWeights mapChildren = (mapWeights, mapChildren)
constructMap (weight:father:tail) index mapWeights mapChildren = constructMap tail (index+1) (Map.insert index weight mapWeights) (addChild mapChildren father index) 

constructTree :: Map Int Int -> Map Int [Int] -> Int -> Tree (Int, Int)
constructTree treeWeights treeChildren root = Node (root, weight) children
    where 
        weight = case (Map.lookup root treeWeights) of Just val -> val
        childrenIndices = case (Map.lookup root treeChildren) of Nothing  -> []
                                                                 Just val -> val
        children = map (constructTree treeWeights treeChildren) childrenIndices

dfs :: Tree (Int, Int) -> Tree (Int, Int)
dfs (Node (index, weight) [])   = Node (index, weight) [] 
dfs (Node (index, weight) children) = Node (index, subweight) children'
    where
        children' = map dfs children
        subweight = findSum children' weight
        findSum :: [Tree (Int, Int)] -> Int -> Int
        findSum [] x   = x
        findSum ((Node (_,weight) _):cs) x = findSum cs (x+weight) 

findMaxChildWeight :: [Tree (Int,Int)] -> Int -> Int
findMaxChildWeight [] x     = x
findMaxChildWeight ((Node (_,w) _):cs) x = findMaxChildWeight cs (max x w)

dfs2 ::  Int -> Tree (Int, Int) -> (Int, Int)
dfs2 totalTreeWeight (Node (index, subWeight) children) = findIndexWeight index myMaxWeight children'
    where    
        myMaxWeight = findMaxChildWeight children (totalTreeWeight - subWeight) 
        children' = map (dfs2 totalTreeWeight) children
        findIndexWeight :: Int -> Int -> [(Int, Int)] -> (Int, Int)
        findIndexWeight minIndexC maxWeightC [] = (minIndexC, maxWeightC)
        findIndexWeight minIndexC maxWeightC ((i,w):tail)
            | maxWeightC > w = findIndexWeight i w tail 
            | maxWeightC == w && i < minIndexC = findIndexWeight i w tail 
            | otherwise = findIndexWeight minIndexC maxWeightC tail

main = do 
     all <- BS.getContents
     let Just (n, r1) = readInt all
     let (x, _)  = readMany readInt r1
     -- let (treeWeights, treeChildren) = constructMap x 1 (Map.empty) (Map.empty)
     -- let root = case (Map.lookup 0 treeChildren) of Just (val:_) -> val
     -- let tree = constructTree treeWeights treeChildren root
     -- let subWeightsTree = dfs tree
     -- let Node (_,totalTreeWeight) _ = subWeightsTree
     -- let (ind, weightttt) = dfs2 totalTreeWeight subWeightsTree
     print (length x :: Int)
     -- let maxWeightsTree = dfs2' subWeightsTree x
     -- print (tree :: Tree (Int, Int))
     -- print (treeWeights :: Map Int Int)
     -- print (treeChildren :: Map Int [Int])
     -- print (subWeightsTree :: Tree (Int, Int))
     -- print (maxWeightsTree :: Tree (Int, Int))
     -- print (ind :: Int)
     -- print (weightttt :: Int)
  where readInt s = BSC.readInt (BSC.dropWhile isSpace s)
        readMany readf s = case readf s of
          Just (x, r) -> let (xs, t) = readMany readf r
                         in  (x : xs, t)
          Nothing     -> ([], s)