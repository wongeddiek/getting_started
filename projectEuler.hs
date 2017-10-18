-- Problem 1: Multiples of 3 and 5
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below n.
sumMult35 :: Int -> Int
sumMult35 n = sum . takeWhile (< n) . filter (\x -> x `mod` 3 == 0 || x `mod ` 5 == 0) $ [1..]

-- Problem 2: Even Fibonacci numbers
-- By considering the terms in the Fibonacci sequence whose values do not exceed n, find the sum of the even-valued terms.
evenFib :: Int -> Int
evenFib n = sum . filter (even) . takeWhile (< n) $ fibs
  where fibs = scanl (+) 0 (1:fibs)

-- Problem 3: Largest prime factor
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number n?

-- function for finding prime factors of a number, outputs a list
findPrimeFactor n factor
  | n <= 1              = []
  | n `mod` factor == 0 = factor : (findPrimeFactor (strip n) (factor+1))
  | otherwise           = findPrimeFactor n (factor+1)
  where strip x = if x `mod` factor == 0 then strip (x `div` factor) else x

-- curried function wrapper for findPrimeFactor, since the starting prime factor is always 2
primeFactors = flip findPrimeFactor 2

-- function wrapper to find the largest prime factor given n
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
