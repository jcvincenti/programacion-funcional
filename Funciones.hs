module Funciones where

id' x = x
doble x = x + x
triple x = 3*x
cuadruple x = 4*x
sumarDos x = x + 2
second = \(a, b) -> b
twice = \f -> \x -> f (f x)
suma = \x -> \y -> x + y
const' = \x -> \y -> x
compose = \f -> \g -> \x -> f(g x)
subst = \f -> \g -> \x -> (f x) (g x)
swap = \(x, y) -> (y, x)
flip' = \f -> \x -> \y -> f y x
uflip = \f -> \p -> f (swap p)
swapC = \x -> \y -> (y, x)
curry' = \f -> \x -> \y -> f (x, y)
uncurry' = \f -> \(x, y) -> f x y
apply = \f -> \x -> f x
swapC' = curry' swap
swap' = uncurry' swapC