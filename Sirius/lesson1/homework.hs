-- Напишите функцию поиска максимума из трех чисел

-- Напишите функцию вычисляющую значение квадратного трехчлена

module HomeWork where

maxtripl x y z = max (max x y) z

max3 x y = max (max x y)

-- maxtripl_lambda x = max (\y z -> max y z) x
maxtripl_lambda x = \y z -> max x (max y z)

sq_eq a b c x = a * x^2 + b * x + c


{-

C = max(S-K, 0) e^(-rT)
S_i = S_(i-1) e^(r - (2/(sigma^2))*T + sigma zT^2
z - нормальное случайное распределение(от 0 до 1)


1. Считать файл с ценой $S_0$, не забыть про проверки открытия файла
2. Посчитать цену на каждом шаге времени $count_tau$ - получить массив цены $S$
3. Посчитать для цены $S_i payoff C$ на каждом шаге
4. Вывести в файл $S$
5. Для вывода в консоль используется цена полученная на последнем шаге $C_n$

-}

call_option s k r t = (max (s - k) 0) * exp ((-r) * t)

stock_price sigma z t s r = s * exp (r - (2 / sigma^2)) * t + sigma * z * t^2

tau = 0
sigma = 0.1
z = 0.1
r = 0.1

blackScholes_eq tau s k r t = if tau == t 
    then call_option s k r t
    else blackScholes_eq (tau + 1) (stock_price sigma z (tau + 1) s r) k r t