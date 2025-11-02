getSecond :: [a] -> a
getSecond a = head (drop 1 a)

getTail :: [a] -> a
getTail a = head (reverse a)

getTailOptionTwo :: [a] -> a
getTailOptionTwo a = head (drop (length a - 1) a)

initEx :: [a] -> [a]
initEx a = reverse (drop 1 (reverse a))

initV2 :: [a] -> [a]
initV2 a = take (length a - 1) a

middle :: [a] -> a
middle a = head (drop (div (length a) 2) a)

checkPalindrome ::  String  -> Bool
checkPalindrome x = (reverse x) == x

checkTriangle :: Float -> Float -> Float -> Bool
checkTriangle a b c = (a < (b+c)) && (b<(a+c)) && (c < (a+b))

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt (s * (s-a)*(s-b)*(s-c))
  where s = (a + b + c) / 2


 {--
    1.6:
        a -> 3
        b -> 4
        c -> 2
        d -> 1
        e -> 8
        f -> 7
        g -> 10
        h -> 9
        i -> 6
        j -> 5
    
    1.7:
        1 + False, 'a' + 'b', 1+2 == "3", 'a' < "ab", (1 <= 2) <= 3, head (1,2)

    1.8: 
        a) [a] -> a
        b) (a,b) -> (b,a)
        c) a -> (a,a)
        d) Num a => a -> a
        e) Fractional a => a -> a
        f) Fractional a => a -> a -> a
        g) Char -> Bool
        h) Ord a => a -> a -> a -> Bool
        i) Eq a => [a] -> Bool
        j) (a -> a) -> a -> a


 --} 