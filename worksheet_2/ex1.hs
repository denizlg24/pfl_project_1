classify :: Int -> String
classify x = if x <= 9 then "failed" else if x >= 10 && x <=12 then "passed" else if x >= 13 && x <= 15 then "good" else if x >= 16 && x <= 18 then "very good" else "excelent"

classify' :: Int -> String
classify' x
  | x <= 9 = "failed"
  | x >= 10 && x <= 12 = "passed"
  | x >= 13 && x <= 15 = "good"
  | x >= 16 && x <= 18 = "very good"
  | otherwise = "excelent"

classifyBMI :: Float -> Float -> String
classifyBMI x y
    | bmi < 18.5 = "underweight"
    | bmi >= 18.5 && bmi < 25 = "normal weight"
    | bmi >= 25 && bmi < 30 = "overweight"
    | otherwise = "obese" 
    where 
        bmi = x / (y^2)

max', min' :: Ord a => a -> a -> a
max' x y = if x>=y then x else y
min' x y = if x<=y then x else y

max3,min3 :: Ord a => a -> a -> a -> a
max3 x y z = max' (max' x y) (max' y z)
min3 x y z = min' (min' x y) (min' x y)

xor :: Bool -> Bool -> Bool
xor True b = not b
xor False b = b

safetail :: [a] -> [a]
safetail [] = []
safetail a = tail a

safetail' :: [a] -> [a]
safetail' a = if null a then [] else tail a

safetail'' :: [a] -> [a]
safetail'' a 
        | null a = []
        | otherwise = tail a

short :: [a] -> Bool
short a = length a < 3

short' :: [a] -> Bool
short' [] = True
short' [x] = True
short' [x,y] = True
short' a = False

median :: (Num a, Ord a) => a -> a -> a -> a
median x y z = (x+y+z) - max3 x y z - min3 x y z

propDivs :: Integer -> [Integer]
propDivs x = [d | d <- [1..x-1], x `mod` d == 0]

perfects :: Integer -> [Integer]
perfects x = [d | d <- [1..x], d == sum(propDivs d)]

pyths :: Integer -> [(Integer,Integer,Integer)]
pyths x = [(a,b,c) | a <- [1..x], b <- [1..x], c <- [1..x], a^2 + b^2 == c^2]

divs :: Integer -> [Integer]
divs x = [d | d <- [1..x], x `mod` d == 0]

isPrime :: Integer -> Bool
isPrime x = length(divs x) == 2

myconcat :: [a] -> [a] -> [a]
myconcat xs ys = [n | ns <- [xs,ys], n<- ns]

myreplicate :: Integer -> a -> [a]
myreplicate n a = [a | n <- [0..n-1]]

myIndex :: [a] -> Integer -> a
myIndex xs i = head [x | (x,j) <- zip xs [0..], j == i]

fac :: Integer -> Integer
fac x = product [1..x]

binom :: Integer -> Integer -> Integer
binom n k = fac n `div` (fac k * fac(n-k))

pascal :: Integer -> [[Integer]]
pascal n = [ [ binom r k | k <- [0..r] ] | r <- [0..n] ]