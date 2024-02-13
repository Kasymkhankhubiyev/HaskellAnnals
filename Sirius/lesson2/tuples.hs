module Tuples where

a = (True, 2::Int)

b = (1, 2, 3)

dist:: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2
