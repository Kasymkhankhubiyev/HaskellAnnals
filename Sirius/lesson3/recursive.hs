module Recursive where


-- recursive implementation of factorial function
factorial n = if n == 0 then 1 else n * factorial (n - 1)


-- we can use polymorphic function
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)


-- double factorial

{-

7!! = 7 * 5 * 3 * 1
8!! = 8 * 6 * 4 * 2
-}

doubleFactorial n = if n <= 1 then 1 else n * doubleFactorial (n-2)


-- handling negative values
factorial'' n = if n < 0 then error "ERROR" 
                         else if n == 0 
                              then 1 
                              else n * factorial'' (n - 1)


-- declaration via secure (protected) expressions 
-- factorial''' 0 = 1
factorial''' n | n < 0 = error "ERROR!"
               | n == 0 = 1
               | n > 0 = n * factorial''' (n - 1)               



-- something like a global variable
factorial5 n | n >= 0 = helper 1 n
             | otherwise = error "arg below zero"

helper acc 0 = acc
helper acc n = helper (acc * n) (n-1)

{-
     helper 1 5
     helper 5 4
     helper 20 3
     helper 60 2
     helper 120 1
     helper 120 0
     ~> factorial 5 = 120
-}

-- `:set +s` shows time and memory amount needded to execute a command


factorial6 n
     | n >= 0 = let
          helper acc 0 = acc
          helper acc n = helper (acc * n) (n - 1)
          in helper 1 n
     | otherwise = error "Error!"

{-
factorial7 n 
          | n >= 0 = helper 1 n
          | otherwise error "Error!"
          where
               helper acc 0 = acc
               helper acc n = helper (acc * n) (n - 1)
-}

