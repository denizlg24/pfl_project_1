import Data.Char (chr, ord, isAlpha, isUpper, isLower)
import Data.List (intercalate)
calcPi1, calcPi2 :: Int -> Double
calcPi1 x = 4 * sum (take x terms)
    where terms = [((-1)**n)/(2*n+1) | n <- [0..]]

calcPi2 x = 3 + sum (take x terms)
    where terms = [(4*((-1)**n))/product [2*n+2 .. 2*n+4] | n <- [0..]]

primes :: [Integer]
primes = sieve [2..]
sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [x | x<-xs, x`mod`p/=0]

twinPrimes :: [(Integer, Integer)]
twinPrimes = [(p, q) | (p, q) <- zip primes (tail primes), q - p == 2]

merge :: [Integer] -> [Integer] -> [Integer]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | x > y     = y : merge (x:xs) ys
    | otherwise = x : merge xs ys

hammingN :: Int -> [Integer]
hammingN n = [2^i * 3^j * 5^k | i <- [0..n], j <- [0..n-i], let k = n - i - j]

hamming :: [Integer]
hamming = concat [hammingN n | n <- [0..]]

hamming' :: [Integer]
hamming' = 1 : (merge(merge (map (2*) hamming') (map (3*) hamming')) (map (5*) hamming'))

rot13Char :: Char -> Char
rot13Char c
  | isUpper c = chr $ (ord 'A') + ((ord c - ord 'A' + 13) `mod` 26)
  | isLower c = chr $ (ord 'a') + ((ord c - ord 'a' + 13) `mod` 26)
  | otherwise = c

rot13 :: String -> String
rot13 = map rot13Char

-- main = do
--   putStrLn "Enter text:"
--   s <- getLine
--   putStrLn ("ROT13: " ++ rot13 s)

type AWord = String
type Line = [AWord]
type Paragraph = [Line]


fillWords :: Int -> [AWord] -> Paragraph
fillWords _ [] = []
fillWords width (w:ws) = fillLine [w] (length w) ws
  where
    fillLine line len [] = [line]
    fillLine line len (x:xs)
      | len + 1 + length x <= width = fillLine (line ++ [x]) (len + 1 + length x) xs
      | otherwise                   = line : fillWords width (x:xs)

-- main :: IO ()
-- main = do
--   input <- getContents
--   putStrLn . unlines . map unwords . fillWords 70 . words $ input

type Dict = [String]
readDict :: IO Dict
readDict = do 
    txt <- readFile "words"
    return (words txt)

-- main = do
--     dict <- readDict
--     print (length dict)

checkWord :: Dict -> String -> String
checkWord ws w
    | contains = w
    | otherwise = "\ESC[7m" ++ w ++ "\ESC[0m"
    where contains = elem w ws

-- main = do
--     putStrLn(checkWord ["good","words"] "bad")

spellCheck :: Dict -> String -> String
spellCheck dict txt =
  unlines (map checkLine (lines txt))
  where
    checkLine line =
      unwords (map (checkWord dict) (words line))

main :: IO ()
main = do
    dict <- readDict
    input <- getContents
    putStrLn (spellCheck dict input)
