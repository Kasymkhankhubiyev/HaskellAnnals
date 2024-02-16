module Task where

import Data.Char

-- splits a string into numeric prefix and other part
readDigits = span isDigit


-- recieves two predicats and recieves a list of elemets
-- that satisfy at list one predicat

filterDisj _ _ [] = []
filterDisj p1 p2 (x:xs)
    | p1 x || p2 x = x : filterDisj p1 p2 xs
    | otherwise = filterDisj p1 p2 xs


filterDisj' p1 p2 = filter (\x -> p1 x || p2 x)

filterDisj'' p1 p2 = filter f
    where f x = p1 x || p2 x


--make a function delAllUpper that deletes all simbols in upper case
checkStr = "Abc is not ABC"
-- delAllUpper = unwords . map (filter isLower) . words
delAllUpper = unwords . filter (any isLower) . words


max3 = zipWith3 (\x y z -> max (max x y) z) [7, 2, 9] [3, 6, 8] [1, 8, 10]


-- Piphagore triangles
xs = [1..20]
triangle = [(x, y, z) | x <- xs, y <- xs, z <- xs, x^2 + y^2 == z^2, x <= y]


-- calculates sum of positive items squares of a given list
sumPositiveSquares:: [Integer] -> Integer
sumPositiveSquares = foldr (\x s -> if x > 0 then x^2 + s else s) 0

-- sumPositiveSquares' = foldr f 0 
--     where f x s = if x > 0 then x^2 else s

-- sumPositiveSquares'' = foldr 


lengthList x = let 
        helper s [] = s
        helper s (x:xs) = helper (s+1) xs  
    in helper 0 x


-- lengthList' = foldr (\_ s -> s+1) 0