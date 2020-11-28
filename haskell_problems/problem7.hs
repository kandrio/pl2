import Prelude

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (h:t)) = (flatten h) ++ (flatten (List t))

flatten' :: NestedList a -> [a]
flatten' l = reverse (myFlatten [] l)
    where 
        myFlatten :: [a] -> (NestedList a) -> [a]
        myFlatten acc (List []) = acc
        myFlatten acc (List ((Elem a):t)) = myFlatten (a:acc) (List t)
        myFlatten acc (List ((List a):t)) = myFlatten (myFlatten acc (List a)) (List t)
