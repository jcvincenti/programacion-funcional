-- Funciones auxiliares

fst = \(a, b) -> a
snd = \(a, b) -> b
suma = \x -> \y -> x + y
--------------------------

-- Ejercicio 1
-- Definir funciones:
-- `curry :: ((a,b) -> c) -> a -> b -> c`
-- `uncurry :: (a -> b -> c) -> (a,b) -> c`
-- de tal manera que:
-- Para toda función `f :: a -> b -> c` , se cumple `curry (uncurry f) = f`
-- Para toda función `f’ :: (a,b) -> c` , se cumple `uncurry (curry f') = f'`

curry = \f -> \a -> \b -> f (a, b)
uncurry = \f -> \(a, b) -> f a b

-- Ejercicio 2
-- Reescribir las siguientes definiciones sin utilizar where , let o lambdas, y utilizando
-- la menor cantidad de paréntesis posible.

-- `apply f = g where g x = f x`
apply f x = f x
-- `twice f = g where g x = f (f x)`
twice f x = f (f x)
-- `id = \x -> x`
id x = x
-- `flip f = g where g x = h where h y = (f y) x`
flip' f x y = f y x
-- `uflip f = g where g p = f (swap p)`
uflip f p = f (swap p)
-- `const = \x -> (\y -> x)`
const x y = x
-- `compose = \f -> (\g -> (\x -> f (g x)))`
compose f g x = f (g x)

-- Ejercicio 3
-- Indicar el tipo de cada una de las funciones del ejercicio anterior, utilizando también
-- la menor cantidad posible de paréntesis.

apply :: (a -> b) -> a -> b
twice :: (a -> a) -> a -> a
id' :: a -> a
flip' :: (a -> b -> c) -> b -> a -> c
uflip :: ((b, a) -> c) -> (a, b) -> c
const' :: a -> b -> a
compose :: (b -> c) -> (a -> b) -> a -> c

-- Ejercicio 4
-- En las expresiones que siguen, colocar los paréntesis que están implícitos,
-- manteniendo el significado de cada una de las expresiones, y dar el tipo de cada una de ellas,
-- suponiendo dadas las definiciones de los ejercicios anteriores.

-- (apply apply) apply
-- (twice doble) 2
-- ((twice twice) twice) swap
-- ((flip twice) 1) doble

-- Ejercicio 5
-- Reescribir las siguientes definiciones utilizando sólo lambdas (sin where ni let ).

appDup f = g where g x = f (x, x)
appDup = \f -> \x -> f (x, x)
appFork (f, g) = h where h x = (f x, g x)
appFork = \(f, g) -> \x -> (f x, g x)
appPar (f, g) = h where h (x, y) = (f x, g y)
appPar = \(f, g) -> \(x, y) -> (f x, g y)
appDist f = g where g (x, y) = (f x, f y)
appDist = \f -> \(x, y) -> (f x, f y)
subst f = h where h g = k where k x = (f x) (g x)
subst = \f -> \g -> \x -> (f x) (g x)

-- Ejercicio 6
-- Indicar cuáles de las siguientes expresiones tienen tipo según el sistema de tipos de Hindley Milner.
-- En el caso de que alguna sea incorrecta, ¿existirá una expresión que utilice las mismas partes,
-- pero asociadas de forma diferente y que sí tenga significado? En el caso de que sí,
-- escribir tal variante.

-- compose (fst snd)
-- compose fst snd :: (a, (b, c)) -> b

-- (uncurry curry snd)
-- uncurry (curry snd) :: (a, b) -> b

-- (apply id) ((id apply) apply)
-- (\x -> x) ((\f -> \x -> f x) apply)
-- (\x -> x) (\x -> apply x)
-- (\x -> apply x)
-- \x -> \z -> x z :: (a -> b) -> a -> b

-- compose (compose doble doble)
-- compose (\x -> doble (doble x))
-- \g -> \y -> (\x -> doble (doble x)) (g y) :: (a -> Int) -> a -> Int

-- (compose compose) doble doble
-- (\g -> \x -> compose (g x)) doble doble
-- (\x -> compose (doble x)) doble
-- (compose (doble doble))
-- No tiene tipo

-- compose (compose doble doble)
-- \g -> \x -> (compose doble doble) (g x)
-- \g -> \x -> (\x -> doble (doble x)) (g x) :: (a -> Int) -> a -> Int

-- compose compose
-- (\g -> \x -> compose (g x))
-- (\g -> \x -> (\h -> \y -> (g x) (h y))) :: (a -> (b -> c)) -> a -> (d -> b) -> d -> c

-- Ejercicio 7
-- Dada la siguiente definición, indicar cómo podría reescribirse usando compose y id :

many :: Int -> (a -> a) -> a -> a
-- many 0 f x = x
many 0 f = id
-- many n f x = f (many (n-1) f x)
many n f x = compose f (many (n-1) f) x


-- Ejercicio 8

-- (Int -> Int) -> (Int -> Int)

-- (a -> (b -> c)) -> (a -> b) -> c

-- (a -> b, c -> d) -> ((a, c) -> (b, d))

-- ((a, a) -> b) -> (a -> b)

-- (a -> (b -> c)) -> (b -> (a -> c))

-- (a -> b) -> ((a, a) -> (b, b))

-- (a -> b, a -> c) -> (a -> (b, c))

-- (a -> (b -> c)) -> ((a -> b) -> (a -> c))

-- a -> (b -> a)

-- Ejercicio 9

cuadruple x = doble (doble x)
timesTwoPlusThree x = suma (doble x) 3
fourTimes f x = f (f (f (f x)))
