module Tasks where



data Color = Red | Green | Blue

instance Show Color where
    show Red = "red"
    show Green = "green"
    show Blue = "blue"


charToInt:: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9
-- charToInt _ = 0


data LogLevel = Error | Warning | Info

cmp:: LogLevel -> LogLevel -> Ordering
cmp Error Warning = GT
cmp Info Warning = LT
cmp Warning Warning = EQ


{-
    Пусть есть тип данных `data Result = Fail | Success`

    и допустим мы определили некотрый дип данных `SomeData ` и 
    некоторую функцию `doSomeWork:: SomeData -> (Result, Int)`
    возвразаюзая результат свокй работы либо код ошибки в случае неудачи, 
    либо 0 в случае успеха

    Определлите функцию `processData`, которая вызывает doSomeWork и возвращает строку "Success"
    в случае ее успешного завершения, либо строку "Fail: N" в случае неудачи, где N - код ошибки 
-}


data Result = Fail | Success
data SomeData = SD | ERR

doSomeWork:: SomeData -> (Result, Int)
doSomeWork SD = (Success, 0)
doSomeWork ERR = (Fail, 1)

processData :: SomeData -> String
processData d = 
    case fst (doSomeWork d) of  
        Success -> "Success"
        Fail -> "Fail: " ++ show (snd (doSomeWork d))


data Point = Pt Double Double deriving Show

origin :: Point
origin = Pt 0.0 0.0

distance :: Point -> Point -> Double
distance (Pt x1 y1) (Pt x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)


{-
    Задача: Определите тип фигуры и найдите ее площадь
-}

data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r^2
area (Rectangle a b) = a * b