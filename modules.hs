import Data.List

-- sum of all third powers that are under 10,000
sum3rdPwr = sum . takeWhile (< 10000) $ map (^3) [1..]

-- function for checking if a sublist is in a List.  Same as function 'isInfixOf'
search sublist list =
  let sublength = length sublist
  in foldl (\acc x -> if take sublength x == sublist then True else acc) False (tails list)

-- since foldl traverses through the entire list, let's rewrite this so it stops once it matches the sublist
search' sublist list = searchList $ tails list
  where sublength          = length sublist
        searchList []     = False
        searchList (x:xs) = if take sublength x == sublist then True else searchList xs
