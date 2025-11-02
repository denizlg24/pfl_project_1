leftHalf :: [a] -> [a]
leftHalf a = take (div (length a) 2) a

rightHalf :: [a] -> [a]
rightHalf a = drop (div (length a) 2) a
