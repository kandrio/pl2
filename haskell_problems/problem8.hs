import Prelude

compress :: Eq a => [a] -> [a]
compress [] = []
compress l = reverse (myCompress [] l)
    where 
        myCompress :: Eq a => [a] -> [a] -> [a]
        myCompress acc [a] = (a:acc)
        myCompress acc (x:y:t) 
            | (x == y) = myCompress acc (y:t)
            | otherwise = myCompress (x:acc) (y:t)  