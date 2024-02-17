module Lab where

{-

Реализуйте метод половинного деления 
https://ru.wikipedia.org/wiki/Метод_бисекции meth_bisection f a b eps

    -- задается: функция, отрезок [a,b], точность
    -- возвращается: (приближенный корень, погрешность, количество итераций)


Реализуйте метод Ньютона (метод касательных)
https://ru.wikipedia.org/wiki/Метод_Ньютона

(простая реализация) meth_newton_n f f’ x0 n

    -- передается: (непрерывная) функция, производная, начальная точка, количество итераций
    -- возвращается: приближенный корень

(правильная реализация) meth_newton f f’ a b x0 eps

    -- задается: функция, производная, [a,b], начальная точка, точность
    -- возвращается: (приближенный корень, погрешность, количество итераций) 
    -- (*доп) выдается error если x_n not in [a,b]

-}


bisection :: (Double -> Double) -> Double -> Double -> Double -> Double
bisection f a b eps = undefined


newTown :: (Double -> Double) -> Double -> Double -> Double -> Double
newTown f a b eps = undefined


newTownRobust :: (Double -> Double) -> Double -> Double -> Double -> Double
newTownRobust f a b eps = undefined
