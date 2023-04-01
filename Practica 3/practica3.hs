-- Ejercicio 1
curry' = \f -> \x -> \y -> f (x, y)
uncurry' = \f -> \(x, y) -> f x y
swap = \(x, y) -> (y, x)
swapC = \x -> \y -> (y, x)
doble x = x + x

-- Ejercicio 2
apply f x = f x
twice f x = f (f x)
id' x = x
flip' f x y = f y x
uflip f p = f (swap p)
const' x y = x
compose f g x = f (g x)

-- Ejercicio 3
apply :: (a -> b) -> a -> b
twice :: (a -> a) -> a -> a
id' :: a -> a
flip' :: (b -> a -> c) -> a -> b -> c
uflip :: ((b, a) -> c) -> (a, b) -> c
const' :: a -> b -> a
compose :: (b -> c) -> (a -> b) -> a -> c

-- Ejercicio 4
-- (apply apply) apply
-- (twice doble) 2
-- ((twice twice) twice) swap
-- ((flip twice) 1) doble

-- Ejercicio 5
appDup = \f -> \x -> f (x, x)
appFork = \(f, g) -> \x -> (f x, g x)
appPar = \(f, g) -> \(x, y) -> (f x, g y)
appDist = \f -> \(x, y) -> (f x, f y)
subst = \f -> \g -> \x -> (f x) (g x)