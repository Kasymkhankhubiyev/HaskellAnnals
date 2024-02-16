module Demo where

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 xs ys zs = sum (take 1 xs ++ [0]) + sum (take 1 ys ++ [0]) + sum (take 1 zs ++ [0]) : 
                    sum3 (drop 1 xs) (drop 1 ys) (drop 1 zs)


{-
    Напишите функцию squares'n'cubes, принимающую список чисел,
    и возвращающую список квадратов и кубов элементов исходного списка.
    GHCi> squares'n'cubes [3,4,5]
    [9,27,16,64,25,125]

    squares'n'cubes :: Num a => [a] -> [a]
    squares'n'cubes = undefined
-}

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes [] = []
squares'n'cubes (x:xs) = x^2 : x^3 : squares'n'cubes xs