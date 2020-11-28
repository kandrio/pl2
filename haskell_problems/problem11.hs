import Prelude

data Element a = Multiple Int a | Single a 
    deriving Show

run :: Eq a => [a] -> [a] -> ([a], [a])
run acc [] = (acc, [])
run (x:xs) (h:t) 
    | h == x = run (h:x:xs) t
    | otherwise = ((x:xs), (h:t))

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack l = reverse (myPack [] l) 
    where 
        myPack :: Eq a => [[a]] -> [a] -> [[a]]
        myPack acc [] = acc 
        myPack acc [h] = ([h]:acc)
        myPack acc (h:g:t) 
            | h == g = myPack (fst:acc) sec 
            | otherwise = myPack ([h]:acc) (g:t) 
                where (fst, sec) = run [h] (g:t) 

myEncode :: Eq a => [Element a] -> [[a]] -> [Element a]
myEncode acc [] = acc
myEncode acc ((x1:xn):xs) = myEncode (x':acc) xs
    where
        x' 
            | len == 1 = Single x1
            | otherwise = Multiple len x1
            where
                len = length (x1:xn)

encodeModified :: Eq a => [a] -> [Element a]
encodeModified [] = []
encodeModified l1 = reverse (myEncode [] l2)
    where
        l2 = pack l1