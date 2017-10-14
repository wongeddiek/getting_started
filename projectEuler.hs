-- Problem 1: Multiples of 3 and 5
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below N.
sumMult35 :: Int -> Int
sumMult35 n = sum . takeWhile (< n) . filter (\x -> x `mod` 3 == 0 || x `mod ` 5 == 0) $ [1..]

-- Problem 2: Even Fibonacci numbers
-- By considering the terms in the Fibonacci sequence whose values do not exceed N, find the sum of the even-valued terms.
evenFib :: Int -> Int
evenFib n = sum . filter (even) . takeWhile (< n) $ fibs
  where fibs = scanl (+) 0 (1:fibs)
