module Operator where

import Prelude

-- set a priority level of an operator
-- with left associative
infixl 6 *+*
(*+*) x y = x * x + y * y

infixl 7 |-|
(|-|) x y = abs(x - y)


--example returns 1 
--('mod' 14)((+5) 10) --> ('mod' 14)(15) --> 15 'mod' 14

-- separator
-- infixr 0 $
-- f $ x = f x


-- we want to declare a function f (g x (h y)) via $ simbol
-- f (g x (h y))
-- f $ g x (h y)
-- f $ g x $ h y


-- rewrite LogBase 4 (min 20 (9+7)) via $
res = logBase 4 $ min 20 $ 9 + 7


--Types
-- (3 :: Int) + (5 :: Integer) -- raises error
-- (3.2 :: Doule) + (5 :: Double) -- Ok

x = 3.2 + 5

