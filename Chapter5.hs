module Chapter5 where

import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift i c
  | isLower c = int2let . (`mod` 26) . (+i) . let2int $ c
  | otherwise = c

encode :: Int -> String -> String
encode i cs = [ shift i c | c <- cs]

type Encoder = String -> String
type Decoder = String -> String

type EncryptionSystem = (Encoder, Decoder)

mkEncryptionSystem :: Int -> EncryptionSystem
mkEncryptionSystem n = (encode n, encode (negate n))

-- Exercises
sumOfSquares :: Int
sumOfSquares = sum [x ^ 2 | x <- [1 .. 100]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m ], y <- [0 .. n]]

square :: Int -> [(Int, Int)]
square n = [coord | coord <- grid n n, fst coord /= snd coord]

myRep :: Int -> a -> [a]
myRep n a = [a | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a, b, c) | a <- [1 .. n], b <- [1 .. n], c <- [1 .. n], a ^ 2 + b ^ 2 == c ^ 2]

factors :: Int -> [Int]
factors n = [i | i <- [1 .. n], n `mod` i == 0]

perfects :: Int -> [Int]
perfects n = [i | i <- [1 .. n], perfect i]
  where perfect int = int == sum (factors int) - int

-- Rewrite this comprehension "[(x, y) | x <- [1, 2], y <- [3, 4]]" in terms of
-- two list comprehensions and one generator


find :: Eq a => a -> [(a, b)] -> [b]
find k vals = [snd b | b <- vals, fst b == k]

-- positions returns a list of all positions at which a value occurs in a list,
-- defined in terms of find
positions :: Eq a => a -> [a] -> [Int]
positions a as = find a (zip as [0 .. ])
