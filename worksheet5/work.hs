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
-- sortBy cmp2 allCards

size :: Set a -> Int
size empty = 0