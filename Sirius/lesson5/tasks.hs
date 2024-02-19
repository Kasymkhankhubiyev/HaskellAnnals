module Tasks where

import Data.Char
import Data.List

-- find x: foldl (-) x [2, 1, 5] == foldr (-) x [2, 1, 5]

res = [x | x <- [1..20], foldl (-) x [2, 1, 5] == foldr (-) x [2, 1, 5]]


{-
    make a fucntion that returns the last element of an array via foldl1
-}

lastEl :: [a] -> a
lastEl = foldl1 (\x y -> y)


{-
    make a function that returns inverse list of simbols that fit a given diapasone
-}

revRange _ _ [] = []
revRange a b (x:xs) = if x >= a && x <= b then x : revRange a b xs else revRange a b xs


{-
    make a generator that generates an array of simbols that 
    fit a given diapasone from a to b
-}

revRange' :: (Char, Char) -> [Char] 
revRange' p = unfoldr g p
    where g (x, y) = if y >= x then Just (y, (x, pred y)) else Nothing


on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

sumSquares3 =  on3 (\x y z -> x + y + z) (^2)


{-
    Задача: Реализуйте класс типов Printable, предоставляющий один метод toString — функцию одной переменной, которая преобразует значение типа, являющегося представителем Printable, в строковое представление.
    Сделайте типы данных Bool и () представителями этого класса типов, обеспечив следующее поведение:
    GHCi> toString True
    "true"
    GHCi> toString False
    "false"
    GHCi> toString ()
    "unit type"

    class Printable a where
    ...

    instance Printable Bool where
    ...

    instance Printable () where
...

-}

class Printable a where
    toString :: a -> String
    -- (a) :: a -> String

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString _ = "unit type"

