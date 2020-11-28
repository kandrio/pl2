import Prelude

data Element a = Multiple Int a | Single a 
    deriving Show

run :: Int -> a -> [a] -> [a]
run 0 a acc = acc
run n a acc = run (n-1) a (a:acc) 

makeList :: Element a -> [a] -> [a]
makeList (Multiple n a) acc = run n a acc
makeList (Single a) acc = a:acc

decode :: [Element a] -> [a] -> [a]
decode [] acc = acc 
decode (x:xs) acc = decode xs acc'
    where
        acc' = makeList x acc

decodeModified :: [Element a] -> [a]
decodeModified elems = reverse (decode elems [])