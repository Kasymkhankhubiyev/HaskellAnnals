module B_S_Opt_Price_formula where

{-
Задание 1. https://ru.wikipedia.org/wiki/%D0%9C%D0%BE%D0%B4%D0%B5%D0%BB%D1%8C_%D0%91%D0%BB%D1%8D%D0%BA%D0%B0_%E2%80%94_%D0%A8%D0%BE%D1%83%D0%BB%D0%B7%D0%B0

Написать модуль вычисляющий цены опциона по формуле Блэка-Шоулса.
Предусмотреть частичное применение функции (от фиксированных параметров).
Добавить тестовые вычисления
-}

-- import Phi
-- phi0 = phi 0   -- 0.5

import qualified Phi -- phi == Phi.phi

--- declaration
priceC = undefined   -- call-option
priceP = undefined   -- put-option

phi0 = Phi.phi 0   -- 0.5


-- test

{-
main = do
    putStrLn ( " p = " ++ (show p) )
    putStrLn $ " fst p = " ++ show (fst p)
--  putStrLn ( show ( fst p ) )
    putStrLn $ show ( fst p ) 
-}


