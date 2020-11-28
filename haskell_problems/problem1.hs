import Prelude

mylast [] = error "No-elements list"
mylast [h] = h
mylast (_:t) = mylast(t)  