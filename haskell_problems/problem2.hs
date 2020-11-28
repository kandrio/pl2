import Prelude

myButLast [] = error "No elements provided in the list, dummy!"
myButLast [_] = error "I'm not gonna do this"
myButLast [h,_] = h
myButLast (_:t) = myButLast t