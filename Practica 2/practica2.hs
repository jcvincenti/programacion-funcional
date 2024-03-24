import Funciones

-- 1) Indicar los tipos de la siguientes definiciones
first::(a, b) -> a
first (x,y) = x

apply::(a -> b) -> a -> b
apply f = g
    where g x = f x
second = \(a, b) -> b
-- twice::(a -> a) -> a -> a
twice f = g
    where g x = f (f x)

doble::Num a => a -> a
doble x = x + x

-- swap::(a, b) -> (b, a)
-- swap (x, y) = (y, x)

uflip:: ((b, a) -> c) -> (a, b) -> c
uflip f = g
    where g p = f (swap p)

-- 2) Indicar el tipo de las siguientes expresiones

-- apply first::(a, b) -> a
-- first (swap, uflip) :: (a, b) -> (b, a)
-- twice doble :: Num a => a -> a
-- twice twice :: (a -> a) -> a -> a
-- twice uflip :: ((a, a) -> c) -> (a, a) -> c
-- twice swap :: (a, a) -> (a, a)
-- uflip swap :: (a, b) -> (a, b)
-- (twice twice) swap :: (b, b) -> (b, b)

appDup = \f -> \x -> f (x, x)
appFork = \(f, g) -> \x -> (f x ,g x)
appPar = \(f, g) -> \(x, y) -> (f x, f y)
appDist = \f -> \(x, y) -> (f x, f y)


--ejercicio7a = appFork (id, id)
--ejercicio7a = \x -> (id x, id x)
ejercicio7a = \x -> (x, x)

--ejercicio7b = \f -> appDup (appDist f)
--ejercicio7b = \f -> appDup (\(x, y) -> (f x, f y))
--ejercicio7b = \f -> \b -> (\(x, y) -> (f x, f y)) (b, b)
ejercicio7b = \f -> \b -> (f b, f b)

--ejercicio7c = appDup id
--ejercicio7c = \x -> id (x, x)
ejercicio7c = \x -> (x, x)

--ejercicio7d = appDup appFork
--ejercicio7d = appDup (\f, g) -> \x -> (f x, g x)
--ejercicio7d = \h -> ((\f, g) -> \x -> (f x, g x)) (h, h)
ejercicio7d = \h -> \x -> (h x, h x)

--ejercicio7e = flip (appDup const)
--ejercicio7e = \a -> \b -> (\x -> \y -> (x, x)) b a
ejercicio7e = \a -> \b -> (b, b)

--ejercicio7f = const (appDup id)
--ejercicio7f = const (\x -> id (x, x))
--ejercicio7f = const (\x -> (x, x))
ejercicio7f = \y -> \x -> (x, x)