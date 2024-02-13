module Digits where
import Data.Char

{-
    the function recieves two chars and if both arguments are digits returns a digit
    combined of these digits in the recieved order, otherwise returns 100
-}

twoDigitsToInt :: Char -> Char -> Int
-- twoDigitsToInt x y = if (&&) (isDigit x) (isDigit y) then digitToInt x * 10 + digitToInt y else 100
twoDigitsToInt x y = if isDigit x && isDigit y then digitToInt x * 10 + digitToInt y else 100