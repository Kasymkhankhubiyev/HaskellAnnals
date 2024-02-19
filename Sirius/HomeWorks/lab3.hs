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
bisection f a b eps = 
    let 
        bisection_ f a b eps counter
            | counter == 10000 =  error ("Iterations limit excided with x = " ++ show x_i)
            | f a == 0 = a
            | f b == 0 = b
            | abs fi < eps = x_i
            | fa * fi > 0 = bisection_ f x_i b eps (counter+1)
            | otherwise = bisection_ f a x_i eps (counter+1)
            where
                fa = f a
                fb = f b
                x_i = (a + b) / 2
                fi = f x_i
            in bisection_ f a b eps 0

newTown :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double
newTown f df x0 eps =
    let 
        newTown_ x0 counter
            | f x0 == 0 = x0
            | counter == 100 = error ("Iterations limit excided with x = " ++ show x0)
            | abs _lambda <= eps = x0
            | otherwise = newTown_ (x0 + _lambda) (counter+1)
            where
                _lambda = (- f x0) / df x0
    in newTown_ x0 0


newTownRobust :: (Double -> Double) -> (Double -> Double) -> Double -> Double -> Double
newTownRobust f df x0 eps = undefined

