import Prelude


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

myEncode :: Eq a => [(Int, a)] -> [[a]] -> [(Int, a)]
myEncode acc (h:l) = myEncode ((length h, head h):acc) l
myEncode acc [] = acc

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode l1 = reverse (myEncode [] l2)
    where
        l2 = pack l1