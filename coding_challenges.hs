-- These are coding challenges that were given in Typescript (JS) and I'm doing them in Haskell

-- Array Challenge: arrayFront9
-- -------------------------------------------------------------------------------------
-- Given an array of numbers, return true if one of the first 4 elements in the array is
-- a 9. The array length may be less than 4.
front9 xs = length [ x| x <- take 4 xs, x == 9 ] > 0


-- Array Challenge: array123
-- -------------------------------------------------------------------------------------
-- Given an array of numbers, return true if the sequence of numbers 1, 2, 3 appears in
-- the array somewhere.
list123 [] = False
list123 [x,y] = False
list123 (x:y:z:xs) = if x == 1 && y == 2 && z == 3 then True else list123 (y:z:xs)


-- String Challenge: same2Chars
-- -------------------------------------------------------------------------------------
-- Given 2 strings, a and b, return the number of the positions where they contain the
-- same length 2 slices. So "xxcaazz" and "xxbaaz" yields 3, since the "xx", "aa",
-- and "az" slices appear in the same place in both strings.
--
-- Bonus Challenge: Solve this problem with recursion! Remember to think about your
-- base case(s), and then implement your recursive call(s)

-- same2Chars :: (Num p) => String -> String -> p
same2Chars [] y = 0
same2Chars x [] = 0
same2Chars x [y] = 0
same2Chars [x] y = 0
same2Chars (xa:xb:xs) (ya:yb:ys) = (if xa:xb:[] == ya:yb:[] then 1 else 0) + same2Chars (xb:xs) (yb:ys)


-- String Challenge: stripX
-- -------------------------------------------------------------------------------------
-- Given a string, return a version where all the "x" have been removed. Except an "x"
-- at the very start or end should not be removed.
--
-- Bonus Challenge: Solve this problem with a regular expression. A slice of (1,-1)
-- might help...

-- stripX :: String -> String
stripX [] = []
stripX [x] = [x]
stripX (x:xs) = [x] ++ [y | y <- init xs, y /= 'x', y /= 'X'] ++ [last xs]


-- String Challenge: stringZipper
-- -------------------------------------------------------------------------------------
-- Given two strings, zip them together into a new string with alternating characters
-- from both string arguments. For example: Given "abc" and "xyz", return "axbycz".
-- Assume the strings have the same length.
stringZipper [] y = y
stringZipper x [] = x
stringZipper (x:xs) (y:ys) = x:y:stringZipper xs ys


--running all the functions with test cases
main = do
  putStrLn "---------- front9 ----------"
  print(front9 [1, 2, 9, 3, 4]) -- true
  print(front9 [1, 2, 3, 4, 9]) --false
  print(front9 [1, 2, 3, 4, 5]) --false
  print(front9 [1, 2, 3, 9, 7]) --true
  print(front9 [1, 9, 3]) --true
  print(front9 [9, 2]) --true
  putStrLn ""

  putStrLn "---------- array123 ----------"
  print (list123 [1, 1, 2, 3, 1]) --true
  print (list123 [1, 1, 2, 4, 1]) --false
  print (list123 [1, 1, 2, 1, 2, 3]) --true
  print (list123 [1, 3, 1, 1, 2, 5]) --false
  print (list123 [1, 2,3,4,5]) --true
  print (list123 [1, 1, 4, 5, 2, 6, 3, 9]) --false
  print (list123 [1, 3, 4, 5, 1, 2, 3, 9]) --true
  putStrLn ""

  putStrLn "---------- same2Chars ----------"
  print (same2Chars "xxcaazz" "xxbaaz") --3
  print (same2Chars "abc" "abc") --2
  print (same2Chars "abc" "axc") --0
  print (same2Chars "iaxxai" "aaxxaaxx") --3
  putStrLn ""

  putStrLn "---------- stripX ----------"
  print ("stripX tests")
  print (stripX "xxHxix")  --"xHix"
  print (stripX "abxxxcd") --"abcd"
  print (stripX "xabxxxcdx") --"xabcdx"
  putStrLn ""

  putStrLn "---------- stringZipper ----------"
  print (stringZipper "abc" "xyz") --"axbycz"
  print (stringZipper "de twr" "osi ok") --"does it work"
  print (stringZipper "GetJb" "ra o!") --"Great Job!"
