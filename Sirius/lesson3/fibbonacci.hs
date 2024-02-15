module Fibbonacci where

fibbonacci 0 = 0
fibbonacci 1 = 1
fibbonacci n = fibbonacci (n - 1) + fibbonacci (n - 2)


fibbonacci' n | n < 0 = fibbonacci' (n + 2) - fibbonacci' (n + 1)
              | n > 1 = fibbonacci' (n - 1) + fibbonacci' (n - 2)
              | n == 1 = 1
              | n == 0 = 0


fibb' n = fibb_ n
    where fibb_ 0 = (0, 1)
          fibb_ 1 = (1, 1)
          fibb_ n | n > 1 = (fst f1 + fst f2, snd f1 + snd f2)
            where 
                f1 = fibb_ (n - 1)
                f2 = fibb_ (n - 2)


--effective
fibb n = fib_ 0 1 n where
    fib_ a b n | n == 0 = a
               | n == 1 = b
               | n >= 2 = fib_ b (a + b) (n - 1)


{-
~> fib 3 
~> helper 2 (2, 1)
~> 2 + 1 + fib 2
~> 2 + 1 + helper 1 (1, 0)

-}


fibb'' n = helper 0 1 n 
    where
        helper a b 0 = a
        helper a b n = helper b (a+b) (n-1)