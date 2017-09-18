doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else doubleMe x
doubleSmallNumber' x = (if x > 100 then x else doubleMe x) + 1

-- function: given an input of list of numbers, output the max number
-- datatype declaration:  a is a contraint of Ord and Num, parameter is [a], output is a
maxList :: (Ord a, Num a) => [a] -> a
maxList [] = error "need a non-empty list!"
maxList [x] = x
maxList (x:y:xs) = if x > y then maxList (x:xs) else maxList (y:xs)
