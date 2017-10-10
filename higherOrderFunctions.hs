-- a function can take a function as a parameter
-- the below takes a function (f) and applies it twice to the second parameter (x)
-- The first parameter (function f) is (a -> a) - the parentheses are required here
-- The second parameter is a, and the function returns a
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- ghci> applyTwice (+3) 10
-- 16
-- ghci> applyTwice (++ " HAHA") "HEY"
-- "HEY HAHA HAHA"
-- ghci> applyTwice ("HAHA " ++) "HEY"
-- "HAHA HAHA HEY"
-- ghci> applyTwice (multThree 2 2) 9
-- 144
-- ghci> applyTwice (3:) [1]
-- [3,3,1]

--zipWith takes a function and two lists as parameters and then joins the two lists by applying the function between corresponding elements.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]
-- [6,8,7,9]
-- ghci> zipWith' max [6,3,2,1] [7,3,1,5]
-- [7,3,2,5]
-- ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
-- ["foo fighters","bar hoppers","baz aldrin"]
-- ghci> zipWith' (*) (replicate 5 2) [1..]
-- [2,4,6,8,10]
-- ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
-- [[3,4,6],[9,20,30],[10,12,12]]

-- Flip takes a function and returns a function that is like our original function, only the first two arguments are flipped.
flip' :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
-- flip' f = g
--   where g x y = f y x
flip' f x y = f y x

-- ghci> flip' zip [1,2,3,4,5] "hello"
-- [('h',1),('e',2),('l',3),('l',4),('o',5)]
-- ghci> zipWith (flip' div) [2,2..] [10,8,6,4,2]
-- [5,4,3,2,1]

-- map takes a function and a list and applies that function to every element in the list, producing a new list.
map' :: (t -> a) -> [t] -> [a]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- filter is a function that takes a predicate (a predicate is a function that tells whether something is true or not, so in our case, a function that returns a boolean value) and a list and then returns the list of elements that satisfy the predicate.
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
-- filter' p (x:xs) = if (p x) then x : filter' p xs else filter' p xs
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

-- let notNull x = not (null x) in filter' notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]

--quicksort using curried function and filter
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted

--  find the largest number under 100,000 that's divisible by 3829.
findNum = head (filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0

-- find the sum of all odd squares that are smaller than 10,000
oddSquares = sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))
--written with list comprehension
-- sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

-- Collatz sequences. We take a natural number. If that number is even, we divide it by two. If it's odd, we multiply it by 3 and then add 1 to that. We take the resulting number and apply the same thing to it, which produces a new number and so on. In essence, we get a chain of numbers. It is thought that for all starting numbers, the chains finish at the number 1
collatz 1 = [1]
collatz n
  | even n = n : collatz (n `div` 2)
  | odd n  = n : collatz (n * 3 + 1)

-- for all starting numbers between 1 and 100, how many chains have a length greater than 15?
collatzTest = length (filter p (map collatz [1..100]))
  where p xs = length xs > 15

-- using lambdas (annoyonmous functions) with the above function:
collatzTest' = length (filter (\xs -> length xs > 15) (map collatz [1..100]))

-- the below maps (*) to a list of numbers starting with 0 and returns a list of functions
-- [(*0), (*1), (*2)...]
-- let listOfFuns = map (*) [0..]
-- calling the index 4 of the above list give you (* 4), passin 5 as the second parameter gives you 20
-- (listOfFuns !! 4) 5

-- function that takes a list of (partially applied) functions and a list and apply each partial funciton element to each element in the 2nd parameter list.  Return the result of each pair in a list.
funZip _ [] = []
funZip (f:fs) (y:xs) = f y : funZip fs xs

-- sum function using foldl
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- using curried function
sum2 :: (Num a) => [a] -> a
sum2 = foldl (+) 0

-- elem function using foldl
elem' y xs = foldl (\acc x -> if x == y then True else acc) False xs

-- map function using foldr
map2 f xs = foldr (\x acc -> f x : acc) [] xs

-- map function using foldl
map3 f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- foldl's binary function parameters are (\acc x), and it iterates the list from left to right.  foldr's binary function parameters are (\x acc), and it iterates the list from right to left.  It makes sense to use foldr to iterate over a list and returns the modified list (ie: map)

-- rewriting standard library functions using folds
maximum' :: (Foldable t, Ord a) => t a -> a
maximum' = foldl1 (\acc x -> if acc >= x then acc else x)

reverse' :: [a] -> [a]
-- reverse' = foldl (\acc x -> x:acc) []
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f = foldr (\x acc -> if f x then x : acc else acc) []

head' :: [a] -> a
head' = foldl1 (\acc _ -> acc)

last' :: [a] -> a
last' = foldr1 (\_ acc -> acc)
