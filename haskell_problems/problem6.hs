import Prelude
import Data.Bool

myReverse' :: [a] -> [a]
myReverse' l = reverse l []
    where
        reverse [] x = x
        reverse (h:l) acc = reverse l (h:acc) 

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = (x == (myReverse' x))