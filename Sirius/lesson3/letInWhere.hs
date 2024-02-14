module LetInWhere where

-- simple function calculating roots
roots :: Double -> Double -> Double -> (Double, Double)
roots a b c =
    (
        (-b - sqrt (b ^ 2 - 4 * a * c))  / 2 / a,
        (-b + sqrt (b ^ 2 - 4 * a * c)) / 2 / a
    )


roots' a b c =
    let d = sqrt (b ^ 2 - 4 * a * c) in ((-b - d) / 2 / a, (-b + d) / 2 / a)

roots'' a b c =
    let
        d = sqrt (b ^ 2 - 4 * a * c)
        g = 2 * a
        x1 = (-b - d) / g
        x2 = (-b + d) / gcd
    in (x1, x2)


-- returns "wow!"
res = (let x = 'w' in [x, 'o', x]) ++ "!"


-- automatic unpacking of tuples
rootsDiff :: Double -> Double -> Double -> Double
rootsDiff a b c = let
    x = roots a b c
    in snd x - fst x


rootsDiff' :: Double -> Double -> Double -> Double
rootsDiff' a b c = let
    (x1, x2) = roots a b c
    in x1 - x2


-- distance calculus via direct tuple unpackaging
dist p1 p2 = sqrt $ (fst p1 - fst p2)^2 + (snd p1 - snd p2)^2

dist' (x1, x2) (y1, y2) = sqrt $ (x1 - y1)^2 + (x2 - y2)^2