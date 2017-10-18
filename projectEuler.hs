-- Problem 1: Multiples of 3 and 5
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below n.

-- first filter an infinite list of natural numbers where each number is multiple of 3 and 5, then end the list when the number is > n, sum the list.
sumMult35 :: Int -> Int
sumMult35 n = sum . takeWhile (< n) . filter (\x -> x `mod` 3 == 0 || x `mod ` 5 == 0) $ [1..]


-- Problem 2: Even Fibonacci numbers
-- By considering the terms in the Fibonacci sequence whose values do not exceed n, find the sum of the even-valued terms.

-- first definte a function to generate an infinite list of fibonacci numbers, end the list when it's > n, filter out all even numbers, and then sum.
evenFib :: Int -> Int
evenFib n = sum . filter (even) . takeWhile (< n) $ fibs
  where fibs = scanl (+) 0 (1:fibs)


-- Problem 3: Largest prime factor
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number n?

-- function for finding prime factors of a number, outputs a list
-- starting with the 1st prime -- 2, if the number n is divisable by the prime p, add to the list.  Then strip n by repeatedly dividing it by p until it's no longer divisible by p.  Then recursively calls the function with the stripped n & p+1.  If n is not divisible by p, call function recursively with n & p+1.
findPrimeFactor :: Int -> Int -> [Int]
findPrimeFactor n factor
  | n <= 1              = []
  | n `mod` factor == 0 = factor : (findPrimeFactor (strip n) (factor+1))
  | otherwise           = findPrimeFactor n (factor+1)
  where strip x = if x `mod` factor == 0 then strip (x `div` factor) else x

-- curried function wrapper for findPrimeFactor, since the starting prime factor is always 2
primeFactors :: Int -> [Int]
primeFactors = flip findPrimeFactor 2

-- function wrapper to find the largest prime factor given n
largestPF :: Int -> Int
largestPF n = last $ primeFactors n


-- Problem 4: Largest palindrome product
-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

-- funciton for determining if a number is a palindrome
isPalin :: Int -> Bool
isPalin n = show n == (reverse $ show n)

-- using list comprehension to find products of elements in 2 lists (going from 90% n - n)
-- filter the output list for only palindrome elements
palinProduct :: Int -> [Int]
palinProduct n = filter (isPalin) [ x * y | x <- [(n * 9 `div` 10)..n], y <- [(n * 9 `div` 10)..n]]

-- palinProduct' :: Int -> Int -> [Int]
-- palinProduct' x y
--   | x <= 899 && y <= 899 = []
--   | x == y               = if isPalin product then product: (palinProduct' 999 (y-1)) else (palinProduct' 999 (y-1))
--   | otherwise            = if isPalin product then product: (palinProduct' (x-1) y) else (palinProduct' (x-1) y)
--   where product = x * y


-- Problem 5: Smallest multiple
-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

-- rewriting the lcm function, given two numbers a, b.  First get list of all multiples of b, filter the list where each multiple is also a multiple of a, take the head of this filtered list
lcm' :: Int -> Int -> Int
lcm' 0 _ = 0
lcm' _ 0 = 0
lcm' a b = head $ filter (\y -> y `mod` a == 0) (map (\x -> x * b) [1..])

-- find the lcm given a list of numbers (x:y:xs).  Find lcm of x,y = z, then recursively call the function with (z:xs)
lcmList []      = 0
lcmList (x:[])   = 0
lcmList (x:y:[]) = lcm x y
lcmList (x:y:xs) = lcmList $ (lcm x y):xs


-- Problem 6: Sum square difference
-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

-- define functions for squaureOfSum and sumOfSquare, take the difference
sumSquareDiff :: [Int] -> Int
sumSquareDiff xs = squaredOfSum xs - sumOfSquared xs
  where squaredOfSum = (^2) . sum
        sumOfSquared = sum . map (^2)
