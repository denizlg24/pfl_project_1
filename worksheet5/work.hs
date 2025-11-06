import Data.List (sortBy)
import Set
data List a = Empty | Cons a (List a)

toList :: [a] -> List a
toList [] = Empty
toList (x:xs) = Cons x (toList xs)

fromList' :: List a -> [a]
fromList' Empty = []
fromList' (Cons x xs) = x: fromList' xs

data Suit = Clubs | Diamonds | Hearts | Spades
    deriving (Show, Eq)

data Face = Numeric Int | J | Q | K | A
    deriving (Show, Eq)

data Card = Card Face Suit
    deriving (Show, Eq)

allCards :: [Card]
allCards = [Card face suit | suit <- [Clubs,Diamonds,Hearts,Spades], face <- [Numeric n | n <- [2..10]] ++ [J,Q,K,A]]

faceValue :: Face -> Int
faceValue (Numeric n) = n
faceValue J = 11
faceValue Q = 12
faceValue K = 13
faceValue A = 14

suitValue :: Suit -> Int
suitValue Clubs = 1
suitValue Spades = 2
suitValue Hearts = 3
suitValue Diamonds = 4


cmp1 :: Card -> Card -> Ordering
cmp1 (Card faceA suitA) (Card faceB suitB)
    | sv1 /= sv2 = compare sv1 sv2
    | otherwise = compare (faceValue faceA) (faceValue faceB)
    where
        sv1 = suitValue suitA
        sv2 = suitValue suitB

cmp2 :: Card -> Card -> Ordering
cmp2 (Card faceA suitA) (Card faceB suitB)
    | fv1 /= fv2 = compare fv1 fv2
    | otherwise = compare (suitValue suitA) (suitValue suitB)
    where
        fv1 = faceValue faceA
        fv2 = faceValue faceB

-- sortBy cmp1 allCards
-- sortBy cmp2 allCardspP

-- main :: IO ()
-- main = do
--   let set1 = foldr insert empty [1..1000]
--   let set2 = fromList [1..1000]
--   print $ height set1
--   print $ height set2

-- set 1 has 1000 height because it keeps inserting empty right
-- set 2 has height 10 because it builds a balanced tree

type AWord = String
type Line = [AWord]
type Paragraph = [Line]

type Dict = [String]
readDict :: IO Dict
readDict = do
    txt <- readFile "words"
    return (words txt)

checkWord :: Set String -> String -> String
checkWord ws w
    | contains = w
    | otherwise = "\ESC[7m" ++ w ++ "\ESC[0m"
    where contains = member w ws

checkWords :: Set String -> [String] -> [String]
checkWords ws = map (checkWord ws)

-- main :: IO ()
-- main = do
--     dict <- readDict  
--     let wordSet = fromList dict  
--     let result = checkWords wordSet ["deniz", "denz", "playo", "pelayo", "big", "tst"]
--     mapM_ putStrLn result

type Name = Char -- ’x’, ’y’, ’z’, etc.
data Prop = Const Bool
        | Var Name
        | Not Prop
        | And Prop Prop
        | Imply Prop Prop
        | Or Prop Prop

vars :: Prop -> [Name]
vars (Var a) = [a]
vars (Not a) = vars a
vars (And a b) = vars a ++ vars b
vars (Imply a b) = vars a ++ vars b
vars (Or a b) = vars a ++ vars b

booleans :: Int -> [[Bool]]
booleans 1 = [[False],[True]]
booleans n = [False:bs | bs <- bss] ++ [True:bs | bs <- bss]
    where bss = booleans (n-1)


type Env = [(Name, Bool)]
environments :: [Name] -> [Env]
environments names = map (zip names) (booleans (length names))


eval :: Env-> Prop -> Bool
eval env (Const b) = b
eval env (Var x)
    = case lookup x env of
        Just b -> b
        Nothing -> error "undefined variable"
eval env (Not p) = not (eval env p)
eval env (And p q) = eval env p && eval env q
eval env (Imply p q) = not (eval env p) || eval env q


table :: Prop -> [(Env,Bool)]
table prop = [(env, eval env prop) | env <- environments (vars prop) ]

satisfies :: Prop -> [Env]
satisfies prop = [env | (env, True) <- table prop]
