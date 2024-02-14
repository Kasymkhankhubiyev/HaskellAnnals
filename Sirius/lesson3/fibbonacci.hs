module Fibbonacci where

fibbonacci 0 = 0
fibbonacci 1 = 1
fibbonacci n = fibbonacci (n - 1) + fibbonacci (n - 2)


fibbonacci' n | n < 0 = fibbonacci' (n + 2) - fibbonacci' (n + 1)
              | n > 1 = fibbonacci' (n - 1) + fibbonacci' (n - 2)
              | n == 1 = 1
              | n == 0 = 0


-- fibbonacci via counter
fibbonacci'' n = 0