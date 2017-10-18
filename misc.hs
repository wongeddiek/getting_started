
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
