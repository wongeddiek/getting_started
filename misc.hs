
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
gcd'' x y = b (mod a b)
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
