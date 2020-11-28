import Prelude

myLength :: [a] -> Int
myLength [] = 0
myLength (_:b) = (myLength b) + 1