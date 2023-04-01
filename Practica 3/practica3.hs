-- Funciones auxiliares

fst = \(a, b) -> a
snd = \(a, b) -> b
suma = \x -> \y -> x + y
--------------------------

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

-- Ejercicio 6

-- compose (fst snd)
-- No tiene tipo. Se podría modificar a:
-- compose fst snd :: (a, (b, c)) -> b

-- (uncurry curry snd)
-- No tiene tipo. Se podria modificar a:
-- uncurry (curry snd) :: (a, b) -> b

-- (apply id) ((id apply) apply)
-- (\x -> x) ((\f -> \x -> f x) apply)
-- (\x -> x) (\f -> \x -> f x)
-- (\f -> \x -> f x)
-- Resultado: (apply id) ((id apply) apply) :: (a -> b) -> a -> b

-- compose (compose doble doble)
-- compose (\x -> doble (doble x))
-- \g -> \x -> (\a -> doble (doble a)) (g x)
-- Resultado: compose (compose doble doble) :: (a -> Int) -> a -> Int

-- (compose compose) doble doble
-- (\g -> \x -> compose (g x)) doble doble
-- (\x -> compose (doble x)) doble
-- compose (doble doble)
-- No tiene tipo. Se podría modificar a:
-- compose (compose doble doble) :: (a -> Int) -> a -> Int
-- Para tipar, tendría que recibir algo como (compose compose) const 5

-- compose compose
-- \g -> \x -> compose (g x)
-- \g -> \x -> \h -> \y -> (g x) (h y)
-- \g -> \x -> \h -> \y -> (g x) (h y) :: (a1 -> b -> c) -> a1 -> (a2 -> b) -> a2 -> c

-- Ejercicio 7

many :: Int -> (a -> a) -> a -> a
-- many 0 f x = x
many 0 f x = id x
-- many n f x = f (many (n-1) f x)
many n f x = compose f (many (n-1) f) x


-- Ejercicio 8

-- (Int -> Int) -> (Int -> Int)
-- (Int -> Int) -> Int -> Int

-- (a -> (b -> c)) -> (a -> b) -> c
-- (a -> b -> c) -> a -> b -> c

-- (a -> b, c -> d) -> ((a, c) -> (b, d))
-- (a -> b, c -> d) -> (a, c) -> (b, d)

-- ((a, a) -> b) -> (a -> b)
-- ((a, a) -> b) -> a -> b

-- (a -> (b -> c)) -> (b -> (a -> c))
-- (a -> b -> c) -> b -> a -> c

-- (a -> b) -> ((a, a) -> (b, b))
-- (a -> b) -> (a, a) -> (b, b)

-- (a -> b, a -> c) -> (a -> (b, c))
-- (a -> b, a -> c) -> a -> (b, c)

-- (a -> (b -> c)) -> ((a -> b) -> (a -> c))
-- (a -> b -> c) -> (a -> b) -> a -> c

-- a -> (b -> a)
-- a -> b -> a

-- Ejercicio 9

cuadruple x = doble (doble x)
cuadruple' x = compose doble doble x
timesTwoPlusThree x = suma (doble x) 3
timesTwoPlusThree' x = (uncurry suma) (doble x, 3)
fourTimes f x = f (f (f (f x)))
fourTimes' f x = many 4 f x
