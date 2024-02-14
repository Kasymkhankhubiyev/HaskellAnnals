-- integration with rectangles
integrateR func a b h = integrateRect a 0.0
    where
        integrateRect x sf
            | x > b = sf * h
            | otherwise = integrateRect (x + h) (sf + func x)
            -- | x > b = s
            -- | otherwise = integrateRect (x + h) (s + h * func x)

test1 = integrateR sin 0 pi 0.1