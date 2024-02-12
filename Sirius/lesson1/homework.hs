-- Напишите функцию поиска максимума из трех чисел

-- Напишите функцию вычисляющую значение квадратного трехчлена


module HomeWork where

maxtripl x y z = max (max x y) z

-- maxtripl_lambda x = max (\y z -> max y z) x
maxtripl_lambda x = \y z -> max x (max y z)

sq_eq a b c x = a * x^2 + b * x + c