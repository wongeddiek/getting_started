
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
