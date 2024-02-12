module Demo where

data B = T | F deriving Show 

add :: Int -> Int -> Int
add a b | a==b = a+a+a | otherwise = a + b