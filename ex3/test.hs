import Prelude

addd' a b = addd'' (2*a) b
    where
        addd'' a b = a + b