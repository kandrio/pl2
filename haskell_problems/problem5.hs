import Prelude

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (h:t) = (myReverse t) ++ [h]

myReverse' :: [a] -> [a]
myReverse' l = reverse l []
    where
        reverse [] x = x
        reverse (h:l) acc = reverse l (h:acc) 