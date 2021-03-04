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

force_eval a = a `seq` a

-- Compute list of (hash_output, hash_input) tuples for range [curr, end] of inputs.
hash_tuple :: (Int -> Int) -> Int -> (Int, Int)
hash_tuple hash input = (hash input, input)

hashes_of_range :: (Int -> Int) -> (Int, Int) -> [(Int, Int)]
hashes_of_range hash (curr, end) = map (hash_tuple hash) [curr..end]

-- Create a list of 'n' (start, end) tuples that represent ranges of input values
-- for the hash function.
ranges _ 1       start end = [(start, end)]
ranges n counter start end = 
    let end' = end `div` n + start - 1
        in 
            ((start, end'):(ranges n (counter-1) (end' + 1) end)) 

parMap' :: NFData b => (a -> b) -> [a] -> MVar () -> Eval [b]
parMap' f [] _ = return []
parMap' f (a:as) signal = do
    sig <- return (tryReadMVar signal) 
    return (if (sig /= Nothing) then [] else let b  = runEval (rpar (force_eval (f a)))
                                                 bs = runEval (parMap' f as signal)
                                                 in (b : bs))
    {-|
    b <- rpar (force_eval (f a))
    bs <- parMap' f as signal
    return (b : bs)
    -}
solver_par :: (Int -> Int) -> [Int] -> MVar () -> MVar [(Int, Int)] -> Int -> IO ()
solver_par hash _ signal box schedulers = do
    let num = schedulers*schedulers*4 
    output <- runEvalIO (parMap' (hashes_of_range hash) (ranges num num 0 (round(2**27-1))) signal)
    let flat_output = concat output
    _ <- readMVar signal
    _ <- putMVar box (flat_output)
    return ()
