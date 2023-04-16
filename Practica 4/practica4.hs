import Data.Char (toLower)
-- Funciones auxiliares

fst = \(a, b) -> a
snd = \(a, b) -> b
suma = \x -> \y -> x + y

doble = \x -> x + x
twice = \f -> \x -> f (f x)

esVocal = \x -> x `elem` ['a', 'e', 'i', 'o', 'u']

esMinuscula = \x -> x == toLower x
--------------------------

-- Ejercicio 1
udiv (x, y) = div x y -- Parcial. udiv(1, 0) es bottom
udivE (x, 0) = error "No puedo dividir por 0" -- Parcial. udiv(1, 0) es bottom
udivE (x, y) = div x y
udivH = uncurry div -- Parcial. udivH(1, 0) es bottom
succ x = x + 1 -- Total
succH = suma 1 -- Total
porLaMitad = flip div 2 -- Total
conDieresis 'u' = 'ü' -- Parcial. conDieresis 'a' es bottom
conDieresisB 'u' = 'ü'
conDieresisB c = conDieresisB c -- Parcial. No termina.
conTildePM 'a' = 'á' -- Parcial. conTildePM 'c' es bottom
conTildePM 'e' = 'é'
conTildePM 'i' = 'í'
conTildePM 'o' = 'ó'
conTildePM 'u' = 'ú'
conTildeE c = if esVocal c -- Parcial. conTildeE 'c' es bottom
    then conTildePM c
    else error "El valor recibido no es vocal"
conTilde c = if esVocal c && esMinuscula c  -- Total
    then conTildePM c
    else c

-- Ejercicio 2
-- udiv = udivH
-- succ = succH

