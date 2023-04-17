import Data.List (elemIndex)
import Data.Maybe
import System.Win32.DebugApi (DebugEventInfo(Exception))
-- Funciones auxiliares

fst = \(a, b) -> a
snd = \(a, b) -> b
suma = \x -> \y -> x + y

doble = \x -> x + x
twice = \f -> \x -> f (f x)
compose = \f -> \g -> \x -> f(g x)
swap = \(x, y) -> (y, x)
--------------------------

-- Ejercicio 1
-- ¿doble = \x -> 2 * x?
-- para todo b. doble b = (\x -> 2 * x) b
--
-- b + b (def doble)   |   2 * b (beta red.)
-- b + b               |   b + b (aritm.)

-- ¿compose doble doble = cuadruple?
-- para todo b. compose doble doble b = cuadruple b
-- 
-- doble (doble b) (def compose)   |   4 * b (def cuadruple)
-- doble (b + b) (def doble)       |   b + b + b + b (aritm.)
-- (b + b) + (b + b) (def doble)   |   (b + b) + (b + b) (aritm.)

-- Ejercicio 2
-- para todo x. para todo y x && y = not ((not x) || (not y))
-- 
-- y x && y   |    not ((not x) || (not y))
-- x && y     |    x && y (ley de morgan)
-- 
-- para todo x. para todo y. not (x || y) = not x && not y
-- 
-- not (x || y)                     |    not x && not y
-- not x && not y (ley de morgan)   |    not x && not y 

-- Ejercicio 3
-- ¿curry suma' = suma?
-- para todo x. para todo y. curry suma' x y = suma x y
-- curry suma' x y            |   suma x y
-- suma' (x, y) (def curry)   |   x + y (def suma)
-- x + y (def suma')          |   x + y (def suma)

-- ¿uncurry suma = suma'?
-- para todo x. para todo y. uncurry suma (x, y) = suma' (x, y)
-- uncurry suma (x, y)     |   suma' (x, y)
-- suma x y (def uncurry)  |   x + y (def suma')
-- x + y (def suma)        |   x + y

-- Ejercicio 4
-- ¿curry fst = const?
-- para todo x. para todo y. curry fst (x, y) = const x y
-- curry fst x y              |   const x y
-- fst (x, y) (def curry)     |   x (def const)
-- x (def fst)                |   x

-- ¿uncurry (flip const) = snd?
-- para todo x. para todo y. uncurry (flip const) (x, y) = snd (x, y)
-- uncurry (flip const) (x, y)   |   snd (x, y)
-- flip const x y (def uncurry)  |   y (def snd)
-- const y x (def flip)          |   y
-- y (def const)                 |   y

-- Ejercicio 5
-- ¿curry (uncurry f) = f?
-- para todo x. para todo y. curry (uncurry f) x y = f x y
-- curry (uncurry f) x y            |   f x y
-- uncurry f (x, y) (def curry)     |   f x y
-- f x y (def uncurry)              |   f x y

-- ¿uncurry (curry f') = f'?
-- para todo x. para todo y. uncurry (curry f') (x, y) = f' (x, y)
-- uncurry (curry f') (x, y)   |   f' (x, y)
-- curry f' x y  (def uncurry) |   f' (x, y)
-- f' (x, y)  (def curry)      |   f' (x, y)
