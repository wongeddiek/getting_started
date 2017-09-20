doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else doubleMe x
doubleSmallNumber' x = (if x > 100 then x else doubleMe x) + 1

-- function: given an input of list of numbers, output the max number
-- datatype declaration:  a is a contraint of Ord and Num, parameter is [a], output is a
maxList :: (Ord a) => [a] -> a
maxList [] = error "need a non-empty list!"
maxList [x] = x
maxList (x:y:xs) = maxList (max x y:xs)

-- rewriting maximum function (does the same as maxList)
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- function for removing all non-uppercase characters
-- removeNonUppercase :: [Char] -> [Char]
removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- function for returning the factorial of a number
factorial :: Integer -> Integer
factorial n = product [1..n]

-- factorial function recursive
factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

-- function to find right triangle where length of side < 10 and parameter = 24
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]

-- function summing each coordinate of the 2 2D vectors
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

-- functions grabbing the 1st, 2nd, and 3rd elements in a triple
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- function returning the first element of a list
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

-- function showing us 1st 3 elements in a list
tell :: (Show a) => [a] -> [Char]
tell [] = "This list is empty."
tell (x:[]) = "This list has one element: " ++ show x ++ "."
tell (x:y:[]) = "This list has two elements: " ++ show x ++ " and " ++ show y ++ "."
tell (x:y:z:_) = "This is a long list.  The first element is " ++ show x ++ ". The second element is " ++ show y ++ ". The third element is " ++ show z ++ "."

-- length function remake
length' :: Num p => [a] -> p
length' [] = 0
length' (_:xs) = 1 + length' xs

-- sum function remake
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- BMI function using guards
-- using 'where' to bind variables
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat    = "You're fat! Lose some weight, fatty!"
  | otherwise     = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)


-- max function using guards
max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

-- compare function using guards
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

-- function taking a list of weight, height paris and return a list of BMIs
-- using where to define a function
bmiList xs = [bmi w h | (w, h) <- xs]
  where bmi w h = (w / h ^ 2)

-- function calculating a cylinder's surface area using 'let..in'
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in  sideArea + 2 * topArea

-- you can use "let..in" anywhere (as it is an expression that returns something) just like "if then else" expressions:
-- 4 * (let a = 9 in a + 1) + 2
-- [let square x = x * x in (square 5, square 3, square 2)]
-- (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)
-- (let (a,b,c) = (1,2,3) in a+b+c) * 100

-- calculate BMI using "let" inside a list comprehension
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- using a predicate to filter out only fat people
-- We can't use the bmi name in the (w, h) <- xs part because it's defined prior to the let binding.
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

calcBmis2 :: (RealFloat a) => [(a, a)] -> [a]
calcBmis2 xs = [let bmi = w / h ^ 2 in bmi | (w, h) <- xs]

-- Case Expressions
-- case expression of pattern -> result
--                    pattern -> result
--                    pattern -> result
--                    ...

-- The follow 2 functions are equivalent
-- head' :: [a] -> a
-- head' [] = error "No head for empty lists!"
-- head' (x:_) = x
--
-- head' :: [a] -> a
-- head' xs = case xs of [] -> error "No head for empty lists!"
--                       (x:_) -> x

-- You can us Case expression in the middle of your function definition:

-- the below two functions are the same:
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
  where what [] = "empty."
        what [x] = "a singleton list."
        what xs = "a longer list."

-- Recursion
-- rewriting the replicate function.  It takes an Int n and some element and returns a list of n numbers of element
replicate' :: (Ord t, Num t) => t -> a -> [a]
replicate' x y
  | x <= 0    = []
  | otherwise = y:(replicate' (x-1) y)

-- rewriting the take function.  Take takes an Int n and a list, and returns the first n elements in the list (as a list)
take' :: (Ord t, Num t) => t -> [a] -> [a]
take' _ [] = []
take' x (y:ys)
  | x <= 0 = []
  | otherwise = y: take' (x-1) ys

-- rewriting the reverse function, it reverses a list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

-- rewriting the repeat function, it repeats an element into an infinite list
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- replicate using take and repeat
replicate'' :: (Ord t, Num t) => t -> a -> [a]
replicate'' x y = take' x (repeat' y)

-- rewriting the zip function, it takes 2 lists and return a list of tuples with 1 element of each list at the same position
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
