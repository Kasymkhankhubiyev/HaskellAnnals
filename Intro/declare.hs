{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Declare where

-- now we make a function in a declarative style
square a b c = sqrt (p * (p - a) * (p - b) * (p-c))
    where p = (a + b + c) / 2

-- there may be multiple arguments in use
square1 a b c = sqrt(p * pa * pb * pc) 
    where
        p = (a + b + c) / 2
        pa = p - a
        pb = p - b
        pc = p - c


-- a function if a composition style
square2 a b c = let p = (a + b + c) / 2
                in sqrt (p * (p-a) * (p-b) * (p-c))

-- or
square3 a b c = let p = (a + b + c) / 2
                in sqrt((let pa = p - a in p * pa)*
                        (let pb = p - b
                             pc = p - c
                             in pb * pc))


-- predicates --prioritised style
hallCapacity :: Int -> String
hallCapacity n
    | n < 10 = "Little"
    | n < 30 = "Medium"
    | otherwise = "Huge"

-- via if-then-else statements
hallCapacity1 :: Int -> String
hallCapacity1 n = if n < 10 
                    then "Little"
                    else (if n < 30
                          then "Medium"
                          else "Huge")


-- no name (or lambda) functions
square_lambda a b c = (\p -> sqrt (p * (p-a) * (p-b) * (p-c))) ((a + b + c) / 2)

-- zips
zip :: [a] -> [b] -> [(a, b)]
zip = zip_With (,)

zip_With :: (a -> b -> c) -> [a] -> [b] -> [c]
zip_With z (a:as) (b:bs) = z a b : zip_With z as bs
zip_With _ _ _ = []
