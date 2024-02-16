module Example where

import Data.List hiding (foldl')

-- правая свертка
foldr' f ini [] = ini
foldr' f ini (x:xs) = f x (foldr' f ini xs)

-- foldr f ini [1, 2, 3] ~> 1 'f' (2 'f' (3 'f' ini))
res1 = foldr (:) [] [1, 2, 3]

res2 = foldr (+) 0 [1, 2, 3]


-- левая свертка

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f ini [] = ini
foldl' f ini (x:xs) = foldl' f (f ini x) xs

-- foldl f ini [1, 2, 3] ~> ((ini 'f' 1) 'f' 2) 'f' 3
res3 = foldl (-) 0 [1, 2, 3]
res4 = foldr (-) 0 [1, 2, 3]


swap p a b = p b a
res5 = foldl (swap (:)) [] [1, 2, 3]


-- scans
-- multiplys from 1 to 10 every next element starting with initial value
res6 = scanl (*) 1 [1..10]

res7 = unfoldr (\x -> if (x <100) then Just (x^2, x+1) else Nothing) 0