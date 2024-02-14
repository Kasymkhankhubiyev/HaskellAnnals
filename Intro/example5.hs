module MappingFiltering where
import Data.Char

-- mapping
a = [1, 2, 3, 4]
b = [True, True, False, False]

m1 = map (^2)

m2 = map (not)

m3 = map ((+) 1)


-- filtering

c = ['1', 'a', '2', 'b', '3', 'c']

d = "Hello World"

f1 = filter (> 2)

f2 = filter even

f3 = filter isDigit

f4 = filter isUpper


-- foldr - свертка

-- sum of the array elements
res1 = foldr (+) 0 a

-- multiplication of the array elentbs
res2 = foldr (*) 1 a

-- max among the array elements
res3 = foldr max (head a) a
