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

