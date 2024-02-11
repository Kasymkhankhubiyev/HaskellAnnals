module Nat where 
import Distribution.Simple.Utils (xargs)

data Nat = Zero | Succ Nat
    deriving(Show, Eq, Ord)

instance Num Nat where
    -- if the second term is 0 return first
    (+) a Zero = a
    -- if the second term is not 0 substruct one fron it
    --and make another iteration
    (+) a (Succ b) = Succ (a + b)
    -- we do not need negative operation
    negate _ = error "negate is undefined for this type class"
    (*) a Zero = Zero
    (*) a (Succ b) = a + (a * b)

    abs x = x
    signum Zero = Zero
    signum _ = Succ Zero

    -- integers overloading
    -- in Haskell integers are overloaded
    fromInteger 0 = Zero
    fromInteger n = Succ (fromInteger(n-1))