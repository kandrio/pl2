import Prelude

rotate :: [a] -> Int -> [a]
rotate l n
    | n > 0 = run n [] l
    | otherwise = reverse (run (-n) [] (reverse l))
    where 
        run _ [] [] = []
        run n l1 (h:t) 
            | n > length (h:t) = error "You are dumb"
            | n > 1 && n <= length (h:t) = run (n-1) (h:l1) t
            | n == 1 = t ++ (reverse (h:l1))
            | n == 0 = (h:t)
        
            -- | n == -1 = (h:l1) ++ t
            -- | otherwise = run (n+1) (h:l1) t
   

