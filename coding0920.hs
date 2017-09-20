-- AP CS Challenge: scoresIncreasing
-- -------------------------------------------------------------------------------------
-- Given an array of scores, return true if each score is equal or greater than the one
-- before.
scoresIncreasing :: Ord a => [a] -> Bool
scoresIncreasing [] = True
scoresIncreasing [x] = True
scoresIncreasing (xa:xb:xs) = xa <= xb && scoresIncreasing (xb:xs)



-- AP CS Challenge: scoresClump
-- -------------------------------------------------------------------------------------
-- Given an array of scores sorted in increasing order, return true if the array
-- contains 3 adjacent scores that differ from each other by at most 2, such as with
-- {3, 4, 5} or {3, 5, 5}.
scoresClump :: (Ord a, Num a) => [a] -> Bool
scoresClump [] = False
scoresClump [_] = False
scoresClump [_,_] = False
scoresClump (xa:xb:xc:xs)
  | diff <= 2    = True
  -- if the list is not sorted by increasing order
  -- | xc - xa <= 2 = True
  | otherwise    = scoresClump (xb:xc:xs)
  where diff = maximum (xa:xb:xc:[]) - minimum (xa:xb:xc:[])


  -- AP CS Challenge: scoresAverage
  -- -------------------------------------------------------------------------------------
  -- Given an array of scores, compute the int average of the first half and the second
  -- half, and return whichever is larger.We'll say that the second half begins at index
  -- length/2. The array length will be at least 2.
scoresAverage :: (Ord p, Fractional p, Real a) => [a] -> p
scoresAverage [] = 0
scoresAverage [x] = 0
scoresAverage x = max list1stAvg list2ndAvg
  where cut        = length x `div` 2
        list1st    = take cut x
        list2nd    = drop cut x
        list1stAvg = realToFrac (sum list1st) / realToFrac (length list1st)
        list2ndAvg = realToFrac (sum list2nd) / realToFrac (length list2nd)


-- AP CS Challenge: wordsCount
-- -------------------------------------------------------------------------------------
-- Given an array of strings, return the count of the number of strings with the given
-- length.
-- wordsCount :: (Num a) => [[Char]] -> Int -> a
wordsCount :: Num a1 => [[a2]] -> Int -> a1
wordsCount [] n = 0
wordsCount (x:xs) n
  | length x == n = 1 + wordsCount xs n
  | otherwise     = 0 + wordsCount xs n

main = do
  print ("scoresIncreasing tests")
  print (scoresIncreasing [1, 3, 4]) -- true
  print (scoresIncreasing [1, 3, 2]) -- false
  print (scoresIncreasing [1, 1, 4]) -- true
  print (scoresIncreasing [1, 1, 2, 4, 4, 7]) -- true
  print (scoresIncreasing [1, 1, 2, 4, 3, 7]) -- false
  print (scoresIncreasing [-5, 4, 11]) -- true
  putStrLn ""

  print ("scoresClump tests")
  print (scoresClump [3, 4, 5]) --true
  print (scoresClump [3, 4, 6]) --false
  print (scoresClump [1, 3, 5, 5]) --true
  print (scoresClump [2, 4, 5, 6]) --true
  print (scoresClump [2, 4, 5, 7]) --false
  print (scoresClump [2, 4, 4, 7]) --true
  print (scoresClump [3, 3, 6, 7, 9]) --false
  print (scoresClump [3, 3, 7, 7, 9]) --true
  print (scoresClump [4, 5, 8]) --false
  putStrLn ""

  print ("scoresAverage tests")
  print (scoresAverage [2, 2, 4, 4]) --4
  print (scoresAverage [4, 4, 4, 2, 2, 2]) --4
  print (scoresAverage [3, 4, 5, 1, 2, 3]) --4
  print (scoresAverage [5, 6]) --6
  print (scoresAverage [5, 4]) --5
  print (scoresAverage [5, 4, 5, 6, 2, 1, 2, 3]) --5
  putStrLn ""

  print ("wordsCount")
  print (wordsCount ["a", "bb", "b", "ccc"] 1) -- 2
  print (wordsCount ["a", "bb", "b", "ccc"] 3) -- 1
  print (wordsCount ["a", "bb", "b", "ccc"] 4) -- 0
  print (wordsCount ["xx", "yyy", "x", "yy", "z"] 1) -- 2
  print (wordsCount ["xx", "yyy", "x", "yy", "z"] 2) -- 2
  print (wordsCount ["xx", "yyy", "x", "yy", "z"] 3) -- 1
