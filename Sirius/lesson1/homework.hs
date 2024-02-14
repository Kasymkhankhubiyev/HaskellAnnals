-- Напишите функцию поиска максимума из трех чисел

-- Напишите функцию вычисляющую значение квадратного трехчлена
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module HomeWork where

import qualified Phi -- phi == Phi.phi

-- maxtripl x y z = max (max x y) z

-- max3 x y = max (max x y)

-- -- maxtripl_lambda x = max (\y z -> max y z) x
-- maxtripl_lambda x = \y z -> max x (max y z)

-- sq_eq a b c x = a * x^2 + b * x + c


{-

C = max(S-K, 0) e^(-rT)
S_i = S_(i-1) e^(r - (2/(sigma^2))*T + sigma zT^2
z - нормальное случайное распределение(от 0 до 1)

2. Посчитать цену на каждом шаге времени $count_tau$ - получить массив цены $S$
3. Посчитать для цены $S_i payoff C$ на каждом шаге
4. Вывести в файл $S$
5. Для вывода в консоль используется цена полученная на последнем шаге $C_n$

-}

sigma = 0.4
z = Phi.phi 0
r = 0.05


blackScholes_eq s k t = let helper s k t tau
                                | tau == t = call_option s k t
                                | otherwise = helper (stock_price (Phi.phi 0.2) (t - 1) s) k t (tau + 1)
                                where
                                call_option s k t = max (s - k) 0 * exp ((-r) * t)
                                stock_price z t s = s * exp (r - (2 / sigma^2) * t) + sigma * z * t^2
                        in helper s k t 1