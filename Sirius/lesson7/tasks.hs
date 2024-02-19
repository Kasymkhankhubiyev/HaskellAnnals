module Tasks where

import Data.Char
import Data.List


{-
    Задача: Реализуйте класс типов Printable, предоставляющий один метод toString — функцию одной переменной, которая преобразует значение типа, являющегося представителем Printable, в строковое представление.
    Сделайте типы данных Bool и () представителями этого класса типов, обеспечив следующее поведение:
    GHCi> toString True
    "true"
    GHCi> toString False
    "false"
    GHCi> toString ()
    "unit type"

    class Printable a where
    ...

    instance Printable Bool where
    ...

    instance Printable () where
    ...

    Сделайте тип пары представителем класса типо Printable

    toString (False, True) -> ("false", "true")
...

-}


class Printable a where
    toString :: a -> String
    -- (a) :: a -> String


instance Printable Bool where
    toString True = "true"
    toString False = "false"


instance Printable () where
    toString _ = "unit type"


instance (Printable a, Printable b) => Printable (a, b) where
    toString (x, y) = "(" ++ toString x ++ ", " ++ toString y ++ ")"


{-
    Задача - реализовать класс типов. обе функции которого ведут себя как succ, pred, но не останавливаются
    при достижении пределом значени, а релизуют цикличность.
-}


-- class SafeEnum a where
--     ssucc :: a -> a
--     spred :: a -> a


-- instance SafeEnum Bool where
--     ssucc True = False
--     ssucc False = True
--     spred True = False
--     spred False = True


-- instance SafeEnum Int where
--     ssucc a = if a == maxBound::Int then minBound::Int else succ a
--     spred a = if a == minBound::Int then maxBound::Int else spred a


class (Enum a, Eq a, Bounded a) => SafeEnum a where
    ssucc :: a -> a
    ssucc x | x == maxBound = minBound 
            | otherwise = succ x

    spred :: a -> a
    spred x | x == minBound = maxBound 
            | otherwise = pred x

instance SafeEnum Bool
instance SafeEnum Int


{-
    implement a function with the following signature 
    avg :: int -> int -> int -> Double
-}

avg :: Int -> Int -> Int -> Double
avg x y z = ((fromIntegral (x + y + z))::Double) / 3.0
