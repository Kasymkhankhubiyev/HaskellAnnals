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

