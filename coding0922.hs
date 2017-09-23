-- Coding exercises creating binary/decimal and hex/decimal converters

-- binary to dec converter.  Takes binary as string
binaryToDec :: [Char] -> Int
binaryToDec [] = 0
binaryToDec (x:xs) = num * 2 ^ (length xs) + binaryToDec xs
  where num = read [x] :: Int

-- decimal to binary converter.  Output binary as string
decToBinary :: (Integral a, Show a) => a -> [Char]
decToBinary n
  | n `div` 2 == 0 = "1"
  | otherwise      = decToBinary (n `div` 2) ++ show (n `mod` 2)

-- hex to dec converter.
hexToDec :: [Char] -> Int
hexToDec [] = 0
hexToDec (x:xs) = num x * 16 ^ (length xs) + hexToDec xs
  where num 'A' = 10
        num 'B' = 11
        num 'C' = 12
        num 'D' = 13
        num 'E' = 14
        num 'F' = 15
        num n   = read [n] :: Int

-- dec to hex converter
decToHex :: (Integral a, Show a) => a -> [Char]
decToHex n
  | n `div` 16 == 0 = hex (n `mod` 16)
  | otherwise       = decToHex (n `div` 16) ++ hex (n `mod` 16)
  where hex 10 = "A"
        hex 11 = "B"
        hex 12 = "C"
        hex 13 = "D"
        hex 14 = "E"
        hex 15 = "F"
        hex x   = show x
