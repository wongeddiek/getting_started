-- 20170921 coding challenges

-- AP CS Challenge: wordsWithoutList
-- -------------------------------------------------------------------------------------
-- Given an array of strings, return a new array where all the strings of the given
-- length are omitted. Hint: An easy way to to this would be to use the Array filter()
-- prototype method.
wordsWithoutList :: Foldable t => [t a] -> Int -> [t a]
wordsWithoutList xs n = [x | x <- xs, length x /= n]


-- AP CS Challenge: hasOne
-- -------------------------------------------------------------------------------------
-- Given a number n, return true if it contains a 1 digit.
hasOne :: Show p => p -> Bool
hasOne n = test xs
  where
    xs = [x | x <- show n, x == '1']
    test [] = False
    test (y:yx) = True


-- AP CS Challenge: dividesSelf
-- -------------------------------------------------------------------------------------
-- We'll say that a number divides itself if every digit in the number divides into the
-- number evenly. So for example 128 divides itself since 1, 2, and 8 all divide into
-- 128 evenly. We'll say that 0 does not divide into anything evenly, so no number with
-- a 0 digit divides itself.
dividesSelf :: Int -> Bool
dividesSelf n
  | testDigits == digits = True
  | otherwise            = False
  where
    digits = [read [x] :: Int | x <- show n]
    testDigits = [y | y <- digits, y /= 0, n `mod` y == 0]


-- AP CS Challenge: copyEvens
-- -------------------------------------------------------------------------------------
-- Given an array of numbers, return a new array of length "count" containing the
-- first even numbers from the original array.
copyEvens :: Integral a => [a] -> Int -> [a]
copyEvens xs n = take n [x | x <- xs, x `mod` 2 == 0]


-- AP CS Challenge: copyEndy
-- -------------------------------------------------------------------------------------
-- We'll say that a number is "endy" if it is in the range 0..10 or 90..100 (inclusive).
-- Given an array of numbers, return a new array of length "count" containing the first
-- endy numbers from the original array. Hint: An array filter method would work well
-- here as well.
copyEndy :: (Ord a, Num a) => [a] -> Int -> [a]
copyEndy xs n = take n [x | x <-xs, x <= 10 || x >= 90]


main = do
  print ("wordsWithoutList tests")
  print (wordsWithoutList["a", "bb", "b", "ccc"] 1) --["bb", "ccc"]
  print (wordsWithoutList["a", "bb", "b", "ccc"] 3) --["a", "bb", "b"]
  print (wordsWithoutList["a", "bb", "b", "ccc"] 4) --["a", "bb", "b", "ccc"]
  print (wordsWithoutList["xx", "yyy", "x", "yy", "z"] 1) --["xx", "yyy", "yy"]
  print (wordsWithoutList["xx", "yyy", "x", "yy", "z"] 2) --["yyy", "x", "z"]
  putStrLn ""

  print ("hasOne tests")
  print (hasOne 10) --true
  print (hasOne 22) --false
  print (hasOne 220) --false
  print (hasOne 212) --true
  print (hasOne 1) --true
  print (hasOne 9) --false
  print (hasOne 211112) --true
  print (hasOne 121121) --true
  print (hasOne 222222) --false
  print (hasOne 56156) --true
  print (hasOne 56556) --false
  putStrLn ""

  print ("dividesSelf tests")
  print (dividesSelf 128) --true
  print (dividesSelf 12) --true
  print (dividesSelf 120) --false
  print (dividesSelf 122) --true
  print (dividesSelf 13) --false
  print (dividesSelf 32) --false
  print (dividesSelf 22) --true
  print (dividesSelf 42) --false
  print (dividesSelf 212) --true
  print (dividesSelf 213) --false
  print (dividesSelf 162) --true
  putStrLn ""

  print ("copyEvens tests")
  print (copyEvens [3, 2, 4, 5, 8] 2) --[2, 4]
  print (copyEvens [3, 2, 4, 5, 8] 3) --[2, 4, 8]
  print (copyEvens [6, 1, 2, 4, 5, 8] 3) --[6, 2, 4]
  print (copyEvens [6, 1, 2, 4, 5, 8] 4) --[6, 2, 4, 8]
  print (copyEvens [3, 1, 4, 1, 5] 1) --[4]
  print (copyEvens [2] 1) --[2]
  print (copyEvens [6, 2, 4, 8] 2) --[6, 2]
  print (copyEvens [6, 2, 4, 8] 3) --[6, 2, 4]
  print (copyEvens [6, 2, 4, 8] 4) --[6, 2, 4, 8]
  print (copyEvens [1, 8, 4] 1) --[8]
  print (copyEvens [1, 8, 4] 2) --[8, 4]
  print (copyEvens [2, 8, 4] 2) --[2, 8]
  putStrLn ""

  print ("copyEndy tests")
  print (copyEndy [9, 11, 90, 22, 6] 2) --[9, 90]
  print (copyEndy [9, 11, 90, 22, 6] 3) --[9, 90, 6]
  print (copyEndy [12, 1, 1, 13, 0, 20] 2) --[1, 1]
  print (copyEndy [12, 1, 1, 13, 0, 20] 3) --[1, 1, 0]
  print (copyEndy [0] 1) --[0]
  print (copyEndy [10, 11, 90] 2) --[10, 90]
  print (copyEndy [90, 22, 100] 2) --[90, 100]
  print (copyEndy [12, 11, 10, 89, 101, 4] 1) --[10]
  print (copyEndy [13, 2, 2, 0] 2) --[2, 2]
  print (copyEndy [13, 2, 2, 0] 3) --[2, 2, 0]
  print (copyEndy [13, 2, 13, 2, 0, 30] 2) --[2, 2]
  print (copyEndy [13, 2, 13, 2, 0, 30] 3) --[2, 2, 0]
