import Prelude

removeAt :: Int -> [a] -> ([a], [a])
removeAt n l = run n [] l
    where 
        run 1 l1 (h:t) = ((reverse l1), t)
        run n l1 (h:t) = run (n-1) (h:l1) t