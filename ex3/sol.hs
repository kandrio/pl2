import Prelude

{-|
fun _ _ num_moves [(0,0)] _ = num_moves 
fun A B num_moves sections curr_speed 
    | curr_speed > lim = -1
    -- | otherwise = minim 100000 (map (fun rest moves+1) speeds)
    | otherwise = min (fun A B (moves+1) rest speed)
        where
            speeds = [carr_speed-B .. curr_speed+A]
            rest   = kati speeds 
-}

-- Νέα περίπτωση
-- Θες, για όλο το εύρος curr_speed' \in [curr_speed-B .. curr_speed+A] 
-- να ψάξεις τον χώρο.
{-|
find_min_moves A B num_moves sections curr_speed = 
    -- minim 1000000 (map (find_min_moves A B (num_moves+1) )) 
    -- min (find_min_moves A B (num_moves+1) sections' speed) (helper A B num_moves sections speeds)
    helper A B num_moves sections speeds
        where
            -- (speed:speeds) = [curr_speed-B .. curr_speed+A]
            speeds = [curr_speed-B .. curr_speed+A]
            -- sections' = update_sections sections speed
            helper A B num_moves sections (s:ss) = 
                min (find_min_moves A B (num_moves+1) sections' speed) (helper A B num_moves sections speeds)
                    where 
                        sections' = update_sections sections speed
                -- ((find_min_moves A B num_moves section' (curr_speed+s)):(helper A B num_moves section curr_speed ss )
-}
{-| 
find_min_moves _ _ _ [(-1,-1)] _ = -1
find_min_moves _ _ num_moves [(0,0)] _ = num_moves
find_min_moves a b num_moves sections curr_speed = 
    helper a b num_moves sections speeds
        where
            speeds = generate_speeds a b curr_speed 
            helper _ _ _ _ [] = 100000000000
            helper a b num_moves sections (speed:speeds) = 
                let x = (find_min_moves a b (num_moves+1) (update_sections sections speed) speed) 
                in (if x == -1 then 1000000000000 else min x (helper a b num_moves sections speeds))
                
                -- min (find_min_moves a b (num_moves+1) sections' speed) (helper a b num_moves sections speeds)
                    --where 
                      --  sections' = update_sections sections speed
-}

find_min_moves _ _ _ [(-1,-1)] _ = -1
find_min_moves _ _ num_moves [(0,0)] _ = num_moves
find_min_moves a b num_moves sections curr_speed = 
    helper a b num_moves sections speeds'
        where
            speeds' = generate_speeds a b curr_speed 
            helper _ _ _ _ [] = -1
            helper a b num_moves sections (speed:speeds) = 
                let x = (find_min_moves a b (num_moves+1) (update_sections sections speed (quot speed 10)) speed) 
                in (if x == -1 then (helper a b num_moves sections speeds) else x)

-- ΘΥΜΗΣΟΥ ΝΑ ΜΗΝ ΠΑΕΙ ΠΟΤΕ ΤΟ SPEED ΣΕ ΑΡΝΗΤΙΚΉ ΤΙΜΉ
-- Βοηθητικές συναρτήσεις

generate_speeds a b curr_speed 
    | (curr_speed - b) <= 0 = [curr_speed+a, curr_speed+a-10 .. 10]
    | otherwise = [curr_speed+a, curr_speed+a-10 .. curr_speed-b]

{-|
generate_speeds a b curr_speed 
    | (curr_speed - b) <= 0 = [10, 20 .. curr_speed+a]
    | otherwise = [curr_speed-b, curr_speed-b+10 .. curr_speed+a]
-}

{-|
update_sections [(0,0)] _ = [(0,0)]
update_sections ((pos,lim):rest) speed = 
    if (speed > lim) then [(-1,-1)] else if (speed' < pos) then ((pos-speed',lim):rest) else (update_sections rest ((speed'-pos)*10))
        where
            speed' = quot speed 10
-}
update_sections [(0,0)] _ _ = [(0,0)]
update_sections ((pos,lim):rest) speed steps = 
    if (speed > lim) then [(-1,-1)] else if (steps < pos) then ((pos-steps,lim):rest) else if (steps == pos) then rest else (update_sections rest speed (steps-pos))

maxim :: Int -> [Int] -> Int
maxim curr_max [] = curr_max
maxim curr_max (x:xs) 
    | x >= curr_max = maxim x xs
    | otherwise = maxim curr_max xs

minim :: Int -> [Int] -> Int
minim curr_min [] = curr_min
minim curr_min (x:xs) 
    | x <= curr_min = minim x xs
    | otherwise = minim curr_min xs

-- Πως κάνεις search σε γραμμικό χώρο στην Haskell;
{-|
mysum b = 5 + b

mymax (x:xs) = mymax' 0 (x:xs)

mymax' curr [] = curr
mymax' curr (x:xs)
    | curr < (mysum x)  = mymax' (mysum x) xs
    | otherwise = mymax' curr xs 
-}


-- Πως μπορείς να κάνεις search σε τετραγωνικό χώρο στην Haskell;
{-|
mysum a b = a+b 

mymax2 listas listbs = 
    maxim 0 (map (mymax' 0 listbs) listas) 

mymax' curr [] _ = curr
mymax' curr (x:xs) a
    | curr < (mysum a x)  = mymax' (mysum a x) xs a
    | otherwise = mymax' curr xs a 
-}





