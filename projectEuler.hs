import Data.Char (digitToInt) --for Problem 8
import Data.List.Ordered (minus, union, unionAll) -- for Problem 10

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

-- rewriting the lcm function, given two numbers x,y, where a is min (abs x y) and b is max (abs x y).  First get list of all multiples of b, filter the list where each multiple is also a multiple of a, take the head of this filtered list
lcm' :: Int -> Int -> Int
lcm' 0 _ = 0
lcm' _ 0 = 0
lcm' x y = head $ filter (\z -> z `mod` a == 0) (map (\z -> z * b) [1..])
  where a = min (abs x) (abs y)
        b = max (abs x) (abs y)

-- find the lcm given a list of numbers (x:y:xs).  Find lcm of x,y = z, then recursively call the function with (z:xs)
lcmList :: [Int] -> Int
lcmList []      = 0
lcmList (x:[])   = 0
lcmList (x:y:[]) = lcm' x y
lcmList (x:y:xs) = lcmList $ (lcm' x y):xs

-- another way to find LCM of two numbers
-- function finding the most occurred prime factors between 2 numbers
primeFactorPairs :: [Int] -> Int -> Int -> [Int]
primeFactorPairs _ 1 1 = []
primeFactorPairs (a:as) x y
  | (x `mod` a) + (y `mod` a) == 0 = a : primeFactorPairs (a:as) (x `div` a) (y `div` a)
  | (x `mod` a) == 0               = a : primeFactorPairs (a:as) (x `div` a) y
  | (y `mod` a) == 0               = a : primeFactorPairs (a:as) x (y `div` a)
  | otherwise                      = primeFactorPairs as x y

-- lcm between 2 numbers is the product of the most occurred prime factors b/w 2 numbers
lcm'' :: Int -> Int -> Int
lcm'' x y = product $ primeFactorPairs [2..] x y

lcmList' :: [Int] -> Int
lcmList' []      = 0
lcmList' (x:[])   = 0
lcmList' (x:y:[]) = lcm'' x y
lcmList' (x:y:xs) = lcmList' $ (lcm'' x y):xs


-- Problem 6: Sum square difference
-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

-- define functions for squaureOfSum and sumOfSquare, take the difference
sumSquareDiff :: [Int] -> Int
sumSquareDiff xs = squaredOfSum xs - sumOfSquared xs
  where squaredOfSum = (^2) . sum
        sumOfSquared = sum . map (^2)


-- Problem 7: 10001st prime
-- What is the 10 001st prime number?

-- function generating an infinite prime list
-- start with 2, attach it with a tail prime list that we'll build.
-- The tail prime list will start at 3, attach it to a filtered list of odd numbers, where the predicate will take this same tail prime list, and check each element in the odd numbers list to each element in this same tail prime list to see if it's divisble (check prime).
-- This can be more efficient.
primesList' :: [Int]
primesList' = 2 : primes'
  where isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
        primes' = 3 : filter (isPrime primes') [5, 7 ..]


--Problem 8: Largest product in a series
-- Find the thirteen adjacent digits in the below 1000-digit number that have the greatest product. What is the value of this product?

longNum = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

-- Grab each consecutive 13-digits number strings from longNum,  pass productNum function -- which coverts each number char to Int and multiply them together, and set them into a new list with the product of each 13-d consec. number.

largestProduct :: Int
largestProduct = maximum (numList longNum)
  where numList []        = []
        numList (x:xs)    =  productNum (take 13 (x:xs)) : numList xs
        productNum        = foldl (\acc x -> acc * (digitToInt x)) 1


-- Problem 9 - Special Pythagorean triplet
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.
--
-- Using the following Pythagorean triplet theorem:
--  When m and n are any two positive integers (m < n):
--
--  a = 2nm
--  b = n^2 - m^2
--  c = n^2 + m^2
--
--  let a + b + c = x, substituting a,b,c with the above
--  2nm + n^2 - m^2 + n^2 + m^2 = x, solve for m
--  nm + n^2 = x/2
--  m + n = x/2n
--  m = x/2n - n, x must be divisible by 2n, m must be > 0
--  conditions for n:
--  x % 2n === 0
--  x/2n - n > 0
--
--  from nm + n^2 = x/2, we get the following condition if n, m are > 0
--  n^2 < x/2

-- function for finding n given the above conditions
findN :: Int -> Int
findN sum' = last $ takeWhile (\n -> (n^2) < (sum' `div` 2)) (filter (\n -> (divisible n) && (greaterZero n)) [1..])
  where divisible a   = sum' `mod` (2*a) == 0
        greaterZero a = sum' `div` (2*a) > 0

-- function for getting a, b, c, and their product
getPyProduct :: Int -> [Int]
getPyProduct sum' = a : b : c : a*b*c : []
  where n = findN sum'
        m = sum' `div` (2*n) - n
        a = 2*n*m
        b = n^2 - m^2
        c = n^2 + m^2

-- Problem 10: Summation of primes
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.
primesList = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+p*2..] | p <- tail primesList])

primesUnder2Mil = sum $ takeWhile (< 2000000) primesList
