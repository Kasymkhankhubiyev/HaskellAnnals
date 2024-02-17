module Lab where

{-
    Реализуйте функцию, находящую значение определённого интеграла от заданной функции f 
    на заданном интервале [a,b] методами правых прямоугольников, трапеций и 
    Симпсона (необязательно).

-}


simpson' :: (Double -> Double) -> Double -> Double -> Double -> Double
simpson' f a b n
    | n < 0 = error "n must be greater than 0"
    | a == b = 0
    | otherwise = simpson_ a 0.0 n
        where
            h = (b - a) / n
            simpson_ x sf n
                | n == 0 = sf * h
                | otherwise = simpson_ (x + h) (sf + (f x + f (x + h) + 4 * f (x + h / 2)) / 6) (n - 1)


rightTriangles' :: (Double -> Double) -> Double -> Double -> Double -> Double
rightTriangles' f a b n
    | n < 0 = error "n must be greater than 0"
    | a == b = 0
    | otherwise = rightTriangles_ b 0.0 n
        where
            h = (b - a) / n
            rightTriangles_ x sf n
                | n == 0 = sf*h
                | otherwise = rightTriangles_ (x - h) (sf + f x) (n - 1)


leftTriangles' :: (Double -> Double) -> Double -> Double -> Double -> Double
leftTriangles' f a b n 
    | n < 0 = error "n must be greater than 0"
    | a == b = 0
    | otherwise = leftTriangles_ a 0.0 n
        where 
            h = (b - a) / n
            leftTriangles_ x sf n
                | n == 0 = sf * h
                | otherwise = leftTriangles_ (x + h) (sf + f x) (n - 1)


trapeze' :: (Double -> Double) -> Double -> Double -> Double -> Double
trapeze' f a b n
    | n < 0 = error "n must be greater than 0"
    | a == b = 0
    | otherwise = trapeze_ a 0.0 n
        where 
            h = (b - a) / n
            trapeze_ x sf n
                | n == 0 = sf * h
                | otherwise = trapeze_ (x + h) (sf + (f x + f (x + h)) / 2) (n - 1)



integrate :: String -> (Double -> Double) -> Double -> Double -> Double -> Double
integrate method f a b n
    | n < 0 = error "n must be greater than 0"
    | a == b = 0
    | method == "RT" = let 
                h = (b - a) / n
                rightTriangles x sf n
                    | n == 0 = sf*h
                    | otherwise = rightTriangles (x - h) (sf + f x) (n - 1)
                in rightTriangles b 0.0 n

    | method == "LT" = let
                h = (b - a) / n
                leftTriangles x sf n
                    | n == 0 = sf * h
                    | otherwise = leftTriangles (x + h) (sf + f x) (n - 1)
                in leftTriangles a 0.0 n

    | method == "TRAP" = let 
                h = (b - a) / n
                trapeze x sf n
                    | n == 0 = sf * h
                    | otherwise = trapeze (x + h) (sf + (f x + f (x + h)) / 2) (n - 1)
                in trapeze a 0.0 n

    | method == "SIM" = let 
            h = (b - a) / n
            simpson x sf n
                | n == 0 = sf * h
                | otherwise = simpson (x + h) (sf + (f x + f (x + h) + 4 * f (x + h / 2)) / 6) (n - 1)
            in simpson a 0.0 n

    | otherwise = error "No method matched, choose from: RT, LT, TRAP, SIM"

f = (2 *)

test1 = integrate "RT" f 0 1 100
test2 = integrate "LT" f 0 1 100
test3 = integrate "TRAP" f 0 1 100
test4 = integrate "SIM" f 0 1 100
