module Discount where

discount limit proc sum = 
    if sum >= limit then sum * (100 - proc) / 100 else sum

-- partial determination
discount_option sum = discount 100 5 sum


-- q q = \q -> \q -> q
-- q :: p1 -> p2 -> p3 -> p3