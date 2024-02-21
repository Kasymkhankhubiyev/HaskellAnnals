module Tasks where

import Data.Char(isDigit)
import Data.List(find)

{-

    Задача: реализуйте фукнции distance, считающую расстояние между двумя точками
    с вещественными координатами, и manhDistance, считающую манхэттенское расстояние
    между двумя точками с целочисленными координатами
-}

data Coord a = Coord a a deriving (Show, Eq)

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x2 - x1) + abs (y2 - y1)


-- ###########

{-

    Задача: Реализуйте функцию getCenter, которая принимает координату ячейки и 
    возвращает координату ее цетра, и функцию getCell, которая принимает координату точки и
    возвращает номер ячейки в которой находится данная точка. В качестве первого аргумента обе 
    эти функции принимают ширину ячейки

    to check:
        p = getCenter 3 (Coord 9 13)
        getCell 3 p

-}

getCenter :: Double -> Coord Int -> Coord Double
getCenter width (Coord x1 x2) = Coord (width * ((fromIntegral x1::Double) + 0.5)) (width * ((fromIntegral x2::Double) + 0.5))

getCell :: Double -> Coord Double -> Coord Int
getCell width (Coord x1 x2) = Coord (floor (x1 / width)) (floor (x2 / width))


-- ##################


{-
    реализуйте функцию `findDigit` которая ищет цифру в строке и возвращает ее,
    если она есть, иначе ничего

-}

findDigit :: [Char] -> Maybe Char
-- findDigit = find isDigit

findDigit [] = Nothing
findDigit (x:xs) | isDigit x = Just x | otherwise = findDigit xs

{-
    реализуйте функцию `findDigitOrX`, использующую предыдущую функцию. которая 
    должна найти цифру в строке или вернуть 'X'
-}

findDigitOrX :: String -> Char
findDigitOrX v = case findDigit v of
    (Just x) -> x
    Nothing -> 'X'


-- #################


{-

    Задача: Тип List эквивалентен определению списков из стандартной бибилиотеки
    в том смысле, что существуют взаимнообратные функции, преобразующие 
    List a в [a] и обратно. Реализуйте эти функции
-}

data List a = Nil | Cons a (List a) deriving Show


fromList :: List a -> [a]
fromList (Cons a Nil) = [a]
fromList (Cons a lst) = a : fromList lst

toList :: [a] -> List a
toList = foldr Cons Nil


{-
    to check:
    toList [1, 2, 3] -> Cons 1 (Cons 2 (Cons 3 Nil))
    fromList (Cons 1 (Cons 2 (Cons 3 Nil))) -> [1, 2, 3]

    toList "abc" -> Cons 'a' (Cons 'b' (Cons 'c' Nil))
    fromList (Cons 'a' (Cons 'b' (Cons 'c' Nil))) -> "abc"

-}


-- #################

{-
    Задача: Есть тип бинарного дерева. Реализуйте функции height и size глубину и размер дерева.

-}

data Tree a = Leaf a | Node (Tree a) (Tree a)


height :: Tree a -> Int
height (Leaf _) = 0
height (Node a b) = 1 + max (height a) (height b)


size :: Tree a -> Int
size (Leaf _) = 0
size (Node a b) = 1 + size a + size b

-- summs all leaves' values
summ :: Tree Int -> Int
summ (Leaf x) = x
summ (Node a b) = summ a + summ b

tr:: Tree Int
tr = Node (Node (Leaf 2) (Leaf 2)) (Leaf 2) -- height 2
tr2 = Node tr (Leaf 4)  -- height 3
tr3 = Node tr2 tr -- height 4


-- #########################



