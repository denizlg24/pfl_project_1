
and' :: [Bool] -> Bool
and' [] = True
and' (x:s) = x && and' s

or' :: [Bool] -> Bool
or' [] = False
or' (x:s) = x && or' s

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

(!!!) :: [a] -> Int -> a
(!!!) xs 0 = head xs
(!!!) xs i = (!!!) (tail xs) (i-1)

elem' :: Eq a => a -> [a] -> Bool
elem' x [a] = x == a
elem' x xs = x == head xs || elem' x (tail xs)

leastDivHelper :: Integer -> Integer -> Integer
leastDivHelper a d
    | d*d > a = a
    | a `mod` d == 0 = d
    | otherwise = leastDivHelper a (d+1)

leastDiv :: Integer -> Integer
leastDiv a = leastDivHelper a 2

isPrimeFast :: Integer -> Bool
isPrimeFast a = (a > 1) && (leastDiv a == a)

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : [y | y <- nub xs, y /= x]

intersperse :: a -> [a] -> [a]
intersperse n [] = []
intersperse n [a] = [a]
intersperse n (x:xs) = (x:[n]) ++ intersperse n xs

insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs)
    | n >= x = x:insert n xs
    | otherwise = n:x:xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort left) (msort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

toBits :: Int -> [Int]
toBits 0 = [0]
toBits 1 = [1]
toBits a = toBits (a `div` 2) ++ [a `mod` 2]

fromBits :: [Int] -> Int
fromBits [] = 0
fromBits (b:bs) = b * 2 ^ length bs + fromBits bs

divisors :: Integer -> [Integer]
divisors n = filter (\d -> n `mod` d == 0 ) [1..n]

isPrimeFast' :: Integer -> Bool
isPrimeFast' n = all (\d -> (n `mod` d) /= 0) [2..floor (sqrt (fromIntegral n))]

myplus :: [a] -> [a] -> [a]
myplus xs ys = foldr (:) ys xs

concat'' :: [[a]] -> [a]
concat'' = foldr (++) []

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

elem'' :: Eq a => a -> [a] -> Bool
elem'' x  = any (== x)

fromBits' :: [Int] -> Int
fromBits' = foldl (\acc bit -> acc * 2 + bit) 0

group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = (x:takeWhile (==x) xs) : group (dropWhile (==x) xs)

intercalate :: a -> [a] -> [[a]]
intercalate a [] = [[a]]
intercalate a xs = [ take i xs ++ [a] ++ drop i xs | i <- [0..length xs] ]

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concatMap (intercalate x) (permutations xs)