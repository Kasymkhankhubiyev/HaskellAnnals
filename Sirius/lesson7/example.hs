-- import Prelude hiding ((==))
module Example where

-- import Prelude (==)

class Eqq a where
    (==) :: a -> a -> Bool
    (\=) :: a -> a -> Bool
    x == y = not (x \= y)
    -- x \= y = not (x == y)

-- instance (Eqq a, Eqq b) => Eqq (a, b) where
    -- p1 == p2 = fst p1 == fst p2 && snd p1 == snd p2

-- instance (Eq a) => Eq [a] where

{-
    check if two lists are equal:
-}

-- res = all (\(x, y) -> x == y) $ zip [1, 2, 3] [4, 5, 6]


-- reading from console:
res = read "10"::Int

res1 = reads "5 types"::[(Int, String)]

-- succ - increment, pred - decrement
res2 = succ 1
res3 = succ False
res4 = succ 'a'

-- some values are bounded - have top and bottom boundaries:
res5 = minBound :: Int
res6 = maxBound :: Char
