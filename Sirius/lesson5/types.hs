module Types where

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f x y = op (f x) (f y)

multSecond = (*) `on` snd