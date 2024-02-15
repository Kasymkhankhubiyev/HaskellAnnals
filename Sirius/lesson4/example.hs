module Example where
import GHC.Real (integralEnumFromThenTo)


{-
f :: b -> c
g :: a -> b
x :: a

fn :: a -> c
fn x = f (g x)

# lambda
fn' = \x -> f (g x)

-}

compose f g = \x -> f (g x)


-- checks if a first item of a tuple is odd
-- (.) has 9 level of priority
oddfst = odd . fst

oddHead = odd . head


-- composition
f = logBase 2
g = (^3)
h = max 42
d = f . g . h


-- filters
-- `filter` - takes values that satisfy a given rule
res1 = filter (<3) [(-1), 0 , 1, 2, 3]
res2 = filter odd [1, 2, 3, 4, 5, 6]
res3 = filter even [1, 3, 5, 7, 9, 11]

-- `takeWhile' - takes elements before faces a first False
res4 = takeWhile odd [1, 3, 5, 7, 8, 9, 11]

-- `dropWhile`- drops elements before meets a first False
res5 = dropWhile odd [1, 3, 5, 4, 5, 7, 9]

-- `span' splits a list when meets a first False
res6 = span odd [1, 3, 5, 4, 5, 7, 9]

-- `break` splits a list when meets a fist True
res7 = break odd [1, 3, 5, 4, 5, 7, 9]
res8 = break even [1, 3, 5, 4, 5, 7, 9]


-- mapping 
res9 = map odd [1, 2, 3, 4, 5, 6, 7]

-- concatenate two lists
res10 = concat [[1, 2, 3], [10, 11, 12]]
res11 = concat ["hello", " world!"]

--concatMap ~ concat . map f
res12 = map (\x -> [x, x]) "ABC"
res13 = concatMap (\x -> [x, x]) "ABC"


-- and or any all - manipulates with lists
-- and returns True if all elements of a list are True
res_1_4 = and [True, True, True, False]
res1_5 = and [True, True, True]

-- or returns True if at list one element of a list is True
res1_6 = or [False, False, True, False]
res1_7 = or []
res1_8 = and []


res1_9 = all even [1, 2, 3, 4, 5, 6, 7]
res2_0 = any even [1, 2, 3, 4, 5, 6, 7]


str = "Hello, my dear friend!"
res2_1 = words str

res2_2 = unwords . map reverse . reverse . words $ str


-- zips 
-- same as simple zip
res2_3 = zipWith (,) [1, 2, 3] "abc"
res2_4 = zipWith (+) [1, 2, 3] [4, 5, 6]


res2_5 = take 10 $ repeat 10
res2_6 = replicate 10 1

res2_7 = take 10 $ cycle "a"
res2_8 = take 10 $ iterate (*2) 2

-- alternative implementation of the repeat function
res2_9 = take 10 $ iterate id 10


-- rangers
res3_0 = [1..10]
res3_1 = enumFromTo 1 10
res3_2 = integralEnumFromThenTo 1 3 10

res3_3 = take 10 $ [1..]

res3_4 = take 10 $ [1, 3..]

xs = [1..20]
res3_5 = [x^2 | x <- xs]
res3_6 = [x^2 | x <- xs, x^2 < 200]


-- as for x in [1, 3] for y in [a, b]
res3_7 = [(x, y) | x <- [1, 3], y <- "ab"]



