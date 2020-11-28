import Prelude

elementAt [] _ = error "You are dumb!"
elementAt (h:_) 1 = h
elementAt (_:t) c 
    | c < 1 = error "You are dumb" 
    | otherwise = elementAt t (c-1)