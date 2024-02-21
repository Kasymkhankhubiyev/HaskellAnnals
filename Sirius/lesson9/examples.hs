module Examples where
import Data.Complex
import Data.Ratio

data Person = Person {firstName :: String, lastName :: String, age :: Int} deriving (Show, Eq)

name :: Person -> String
name person = firstName person ++ " " ++ lastName person

-- name':: Person -> String
-- name':: (Person {lastName = ln, firstName = fn}) = fn ++ " " ++ ln

-- name'':: Person -> String
-- name'':: (Person fn ln _) = fn ++ " " ++ ln

data CoordD = CoordD Double Double
data CoordI = CoordI Int Int

data Coord a = Coord a a

twice :: a -> [a]  -- twice :: a -> [] a
twice x = [x, x]

thrice :: a -> (,,) a a a
thrice x = (,,) x x x

{-
    data Maybe a = Nothing | Just a
    data Either a b = Left a | Right b

-}

root :: Double -> Double -> Double -> Either [Char] (Double, Double)
root a b c 
    | discr >= 0 = Right (((-b) - sqrt discr) / 2 / a, ((-b) - sqrt discr )/ 2 / a)
    | otherwise  = Left "Negative discriminant"
    where
        discr = b^2 - 4 * a * c


{-
    Strict 

    example: 
        1. getXLazy (CoordLazy 1 undefined) returns x
        2. getXStrict (CoordStric 1 undefined) returns exception while comstructing
-}

data CoordLazy a = CoordLazy a a deriving Show
data CoordStric a = CoordStric !a !a deriving Show

getXLazy :: CoordLazy a -> a
getXLazy (CoordLazy x _) = x

getXStrict :: CoordStric a -> a
getXStrict (CoordStric x _) = x


data List a = Nil | Cons a (List a) deriving Show
