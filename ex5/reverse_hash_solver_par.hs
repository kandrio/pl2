import Control.Concurrent
import Control.Parallel.Strategies
import Control.DeepSeq
import System.Random
import System.Timeout
import Data.Hashable (hashWithSalt)
import Data.List (nub)
import Data.Time
import Data.Maybe
import qualified Data.Set as S
import Text.Printf (printf)
-- run main program with: ghc -cpp -O2 reverse_hash.hs -threaded -rtsopts && ./reverse_hash +RTS -N8


-- Compute list of (output, input) tuples for range [curr, end] of inputs
-- 1st way
{-|
hashes_of_range :: Int -> Int -> (Int -> Int) -> [(Int, Int)]
hashes_of_range curr curr _    = [(hash curr, curr)]
hashes_of_range curr end  hash = ((hash curr, curr):(hashes_of_range (curr+1) end hash))
-}

-- 2nd way
hash_tuple :: (Int -> Int) -> Int -> (Int, Int)
hash_tuple hash input = (hash input, input)

hashes_of_range :: (Int -> Int) -> (Int, Int) -> [(Int, Int)]
hashes_of_range hash (curr, end) = map (hash_tuple hash) [curr..end]
-------------------------------------------------------------------------------------------

-- Create a list of 'n' (start, end) tuples that represent ranges of inputs for the hash function
ranges _ 1       start end = [(start, end)]
ranges n counter start end = 
    let end' = end `div` n + start - 1
        in 
            ((start, end'):(ranges n (counter-1) (end' + 1) end)) 


parMap' :: (a -> b) -> [a] -> Eval [b]
parMap' f [] = return []
parMap' f (a:as) = do
    b <- rpar (f a)
    bs <- parMap' f as
    return (b : bs)

solver_par :: (Int -> Int) -> [Int] -> MVar () -> MVar [(Int, Int)] -> Int -> IO ()
solver_par hash _ signal box schedulers = do
    let pieces = schedulers*schedulers*4
    output <- runEvalIO (parMap' (hashes_of_range hash) (ranges pieces pieces 0 (round(2**27-1))))
    let flat_output = concat output
    -- print (length (flat_output))
    -- _ <- readMVar signal
    putStrLn "I GOT THERE!"
    _ <- putMVar box (deep flat_output)
    putStrLn "I GOT THERE ALSO"
    return ()

deep a = deepseq a a

{-|

solver_par :: (Int -> Int) -> [Int] -> MVar () -> MVar [(Int, Int)] -> Int -> IO ()
solver_par hash _ signal box schedulers = do 
    -- _ <- readMVar signal
    _ <- putMVar box [deep (hash 1, 1)]
    parMap' (f box hash) [2..2^27-1]


-- Δεν δουλεύει σωστά 
f :: MVar [(Int, Int)] -> (Int -> Int) -> Int -> IO ()
f box hash input = do
    --t <- readMVar box --test
    --putMVar box ((111110,11212121):t) --test
    t <- takeMVar box
    putMVar box ((deep (hash input, input)):t)


deep :: (Int, Int) -> (Int, Int)
deep a = deepseq a a

parMap' :: (Int -> IO ()) -> [Int] -> IO ()
parMap' f [a] = runEval (rpar (f a))
parMap' f (a:as) = do
    runEval (rpar (f a)) -- IO ()
    parMap' f as  
-}