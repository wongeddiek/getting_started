-- Logic Challenge: cigarParty
-- -------------------------------------------------------------------------------------
-- When squirrels get together for a party, they like to have cigars. A squirrel party
-- is successful when the number of cigars is between 40 and 60, inclusive. Unless it
-- is the weekend, in which case there is no upper bound on the number of cigars.
-- Return true if the party with the given values is successful, or false otherwise.
cigarParty :: (Ord a, Num a) => a -> Bool -> Bool
-- using pattern matching
cigarParty cigars False = cigars >= 40 && cigars <= 60
cigarParty cigars True = cigars >= 40
-- not using pattern matching
-- cigarParty cigars weekend = cigars >= 40 && (weekend || cigars <= 60)


-- Logic Challenge: squirrelPlay
-- -------------------------------------------------------------------------------------
-- The squirrels in Cheyenne spend most of the day playing. In particular, they play if
-- the temperature is between 60 and 90 (inclusive). Unless it is summer, then the upper
-- limit is 100 instead of 90. Given an int temperature and a boolean isSummer, return
-- true if the squirrels play and false otherwise.
squirrelPlay :: (Ord a, Num a) => a -> Bool -> Bool
-- using pattern matching
squirrelPlay temp False = temp >= 60 && temp <= 90
squirrelPlay temp True = temp >= 60 && temp <= 100
-- not using pattern matching
-- squirrelPlay temp isSummer = temp >= 60 && (isSummer && temp <= 100 || temp <= 90)


-- Logic Challenge: alarmClock
-- -------------------------------------------------------------------------------------
-- Given a day of the week encoded as 0=Sun, 1=Mon, 2=Tue, ...6=Sat, and a boolean
-- indicating if we are on vacation, return a string of the form "7:00" indicating when
-- the alarm clock should ring. Weekdays, the alarm should be "7:00" and on the weekend
-- it should be "10:00". Unless we are on vacation -- then on weekdays it should be
-- "10:00" and weekends it should be "off".
alarmClock :: Integral a => a -> Bool -> [Char]
alarmClock day True = if (day `mod` 6 > 0) then "10:00" else "off"
alarmClock day False = if (day `mod` 6 > 0) then "7:00" else "10:00"


-- arrayConcat
-- -------------------------------------------------------------------------------------
-- Given two arrays, return a new array with both of them concatenated together.
-- For example, given [1,2,3] and [4,5,6] return [1,2,3,4,5,6].
-- arrayConcat :: [a] -> [a] -> [a]
-- arrayConcat [] y = y
-- arrayConcat x y = arrayConcat (init x) (last x:y)

--using foldr
arrayConcat xs ys = foldr (:) ys xs

-- /*
--
-- Array Challenge: arrayForEach
-- -------------------------------------------------------------------------------------
-- Much like the previous arrayConcat challenge, rebuild the Array protoype method
-- forEach(), without using the built-in forEach method! Read up on forEach here:
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/forEach
--
-- Hint: All you really need to do is use a simple for loop and to call the callback
-- function!
--
-- */
arrayForEach _ [] = []
arrayForEach f (x:xs) = f x : arrayForEach f xs


main = do
  print ("cigarParty tests")
  print(cigarParty 30 False) --false
  print(cigarParty 50 False) --true
  print(cigarParty 70 False) --false
  print(cigarParty 70 True) --true
  putStrLn ""

  print ("squirrelPlay tests")
  print(squirrelPlay 70 False) --true
  print(squirrelPlay 95 False) --false
  print(squirrelPlay 95 True) --true
  putStrLn ""

  print ("alarmClock tests")
  print (alarmClock 1 False) --"7:00"
  print (alarmClock 5 False) --"7:00"
  print (alarmClock 0 False) --"10:00"
  print (alarmClock 0 True) --"off"
  print (alarmClock 1 True) --"10:00"
  print (alarmClock 6 True) --"off"
  putStrLn ""

  print ("arrayConcat tests")
  print (arrayConcat [1, 2, 3] [4, 5, 6]) --[1,2,3,4,5,6]
  print (arrayConcat [3] [1, 4, 1, 5, 9]) --[3,1,4,1,5,9]
  print (arrayConcat [2, 7] [1, 8, 2, 8]) --[2,7,1,8,2,8]
  print (arrayConcat [1, 4, 1, 4] [2, 1, 3]) --[1,4,1,4,2,1,3]
  putStrLn ""

  print ("arrayForEach tests")
  print (arrayForEach (* 2) [1, 2, 3] )
