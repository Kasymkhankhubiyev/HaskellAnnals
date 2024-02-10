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


