module Logic where

import Prelude(Bool(..), Show(..), Eq(..))

true :: Bool
true = True

false :: Bool
false = False

-- the function recieves one argument of type boolean
not :: Bool -> Bool
-- returns False if True
not True = False
-- returns True if False
not False = True

-- the function recieves two arguments of type boolean
and :: Bool -> Bool -> Bool
-- returns False if one of arguments if False, so if first if False,
-- no metter what if the second argument
and False _ = False
-- returns the value of the second argument if the first is True, cause
-- the final value if True if only the second is true and False otherwise
and True x = x


-- the function recieves two arguments of boolean type
or :: Bool -> Bool -> Bool
-- the function returns False if both arguments are False and True otherwise
or True _ = True
or False x = x


-- the function recieves two arguments  of boolean type
-- and returns one value of boolean type
xor :: Bool -> Bool -> Bool
-- here we use previously defined functions to define xor
xor a b = or (and (not a) b) (and a (not b))

-- realization of IF-THEN_ELSE structure
ifThenElse :: Bool -> a -> a -> a
ifThenElse True t _ = t
ifThenElse False _ e = e

