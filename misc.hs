import Data.List --(\\)
import Data.List.Ordered (minus, union, unionAll)

-- Given a list, for each element, return a list that contains the all elements from start of the input list to the current element.
combinator :: [a] -> [[a]]
combinator [] = []
combinator (x:xs) = scanl (\acc x -> acc ++ [x]) [x] xs

-- rna transcription - from exercism
rna_tran xs = [ map x | x <- xs , x == 'G' || x == 'C' || x == 'T' || x == 'A']
  where map 'G' = 'C'
        map 'C' = 'G'
        map 'T' = 'A'
        map 'A' = 'U'

-- given a list, check if the list is a palindrome
listPal :: Eq a => [a] -> Bool
listPal [x] = True
listPal [x,y] = x == y
listPal (x:xs) = x == last xs && (listPal $ init xs)


-- rewriting the GCD Function
-- Euclid's GCD Algorithm: given two numbers x and y, take the GCD of abs diff (x y) and min x y, if GCD x = y, return x
gcd' :: Int -> Int -> Int
gcd' x 0 = abs x
gcd' 0 y = abs y
gcd' x y
  | x == y     = abs x
  | otherwise  = gcd' a b
  where a = abs (x - y)
        b = abs $ min x y

-- Euclid's GCD algorithm using `mod`
gcd'' :: Int -> Int -> Int
gcd'' x 0 = abs x
gcd'' 0 y = abs y
gcd'' x y = gcd'' b (mod a b)
  where a = abs x
        b = abs y


-- given two number x and y, create a list from (min x y) to 1, filter the list so it's divisible by both x and y, then take the head of the list
gcdLong x 0 = abs x
gcdLong 0 y = abs y
gcdLong x y = head $ divisors a b
  where a = abs x
        b = abs y

divisors :: Int -> Int -> [Int]
divisors x y = filter (\ z -> ((mod x z) + (mod y z)) == 0)  [a, b..1]
  where a = min x y
        b = if (a - 1) < 0 then 0 else (a - 1)

-- Prime sieve - inefficient
primesTo m = sieve [2..m]       {- (\\) is set-difference for unordered lists -}
   where sieve (x:xs) = x : sieve (xs \\ [x,x+x..m])
         sieve []     = []

-- prime sieve - more efficient
-- infinite prime list.  Uses the Data.List.Ordered module -> minus, unionAll functions

-- The minus function computes the difference of two ordered lists. An element occurs in the output as many times as it occurs in the first input, minus the number of occurrences in the second input. If the first input is a set, then the output is a set.  It computes for infinite lists

-- The unionAll computes the union of a (potentially) infinite number of lists, under the assumption that the heads of the inner lists are sorted. The result will duplicate an element as many times as the maximum number of occurrences in any single list. Thus, the result is a set if and only if every inner list is a set.

--Start with the first 2 primes, next generate a nested list of all odd prime multiples, starting with prime^2 using list comprehension, beginning with 3.  Computes the union of this nested list using 'unionAll'.  Then, using a list of all odd numbers starting 5, compute the difference with the multiple list using 'minus' to remove all the odd prime multiples.

primesList = 2 : 3 : minus [5,7..] (unionAll [[p*p, p*p+p*2..] | p <- tail primesList])


-- Generate an infinite prime list
-- start with 2, attach it with a tail prime list that we'll build.
-- The tail prime list will start at 3, attach it to a filtered list of odd numbers, where the predicate will take this same tail prime list, and check each element in the odd numbers list to each element in this same tail prime list to see if it's divisble (check prime)
primesList' = 2 : primes'
  where isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
        primes' = 3 : filter (isPrime primes') [5, 7 ..]
