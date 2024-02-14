module Lists where


-- adds 42 to a list head
cons = (42:)

-- adds two given elements into list head
addTwoElements a b lst = a : b : lst


-- inserts a value into list n times
nTimes a n
    | n > 0 = let 
        helper a 0 lst = lst
        helper a n lst = helper a (n-1) (a : lst)
        in helper a n []
    | n == 0 = []
    | otherwise = error "Error"


-- alternative imolementation
nTimes' :: a -> Int -> [a]
nTimes' e 0 = []
nTimes' e 1 = [e]
nTimes' e n = e : nTimes' e (n-1)


-- get second value of a list
second xs = head (tail xs)

head' ((:) x xs) = x
head'' (x:xs) = x
tail' ((:) x xs) = xs
tail'' (x:xs) = xs

second' (y:x:xs) = x
second'' (_:x:_) = x


-- returns a second value of the head of a list of tuples
-- [(1, 2), (3, 4), (5, 6)]
sndHead x = snd (head x)
sndHead' ((_, x):_) = x


-- something like mapping
sq [] = []
sq (x:xs) = (x^2) : sq xs


map' f [] = []
map' f (x:xs) = (f x) : map' f xs


oddsOnly [] = []
oddsOnly (x:xs) = if odd x then x : oddsOnly xs else oddsOnly xs

oddsOnly' [] = []
oddsOnly' (x:xs)
    | odd x = x : oddsOnly' xs
    | otherwise = oddsOnly' xs


evenOnly [] = []
evenOnly (x:xs) = if even x then x : evenOnly xs else evenOnly xs 

-- oddOrEvenOnly f [] = []
-- oddOrEvenOnly f (x:xs) = if (f x) then x : oddOrEvenOnly xs else oddOrEvenOnly xs


reverse' :: [a] -> [a]
reverse' l = rev l [] where
    rev [] a = a
    rev (x:xs) a = rev xs (x:a)

{-
reverse [1, 2, 3]
~> rev [1, 2, 3] []
~> rev [2, 3] (1:[])
~> rev [3] (2:1:[])
~> rev [] (3:2:1:[])
~> (3:2:1:[])
~> (3:2:[1]) 
~> 3:[2, 1] 
~> [3, 2, 1]
-}


-- isPolindrom :: (Eq a, Ord[a]) => [a] -> Bool
-- isPolindrom [a] = True
-- isPolindrom [] = error "empty array"
isPolindrom str = str == reverse str


zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a, b) : zip as bs

zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' (a:as) (b:bs) (c:cs) = (a, b, c) : zip3' as bs cs
zip3' _ _ _ = []

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((x, y):xys) = let (xs, ys) = unzip xys 
                        in (x:xs, y:ys)

{-
unzip [(1, 'a'), (2, 'b), (3, 'c')]
~> x = 1, y = 'a', xys = [(2, 'b), (3, 'c')] | unzip [(2, 'b), (3, 'c')]
~> x = 2, y = 'b', xys = [(3, 'c')] | unzip [(3, 'c')]
~> x = 3, y = 'c', xys = [] | unzip []
~> x = 3, y = 'c', xys = [] | ([], [])
~> x = 3, y = 'c', xs = [], ys = [] | ([], [])
~> x = 3, y = 'c', xs = [], ys = [] | (3:[], 'c':[])
~> x = 2, y = 'b', xs = 3:[], ys = 'c':[] | (2:3:[], 'b':'c':[])
~> x = 1, y = 'a', xs = 2:3:[], ys = 'b':'c':[] | (1:2:3:[], 'a':'b':'c':[])
~> (1:2:3:[], 'a':'b':'c':[])
~> (1:2:[3], 'a':'b':['c'])
~> (1:[2, 3], 'a':['b', 'c'])
~> ([1, 2, 3], ['a', 'b', 'c'])
-}


-- drop, split, take
