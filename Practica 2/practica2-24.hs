-- import Funciones

-- Ejercicio 1
-- Indicar los tipos de las siguientes definiciones:

first :: (a, b) -> a
first (x,y) = x

apply :: (a -> b) -> (a -> b)
apply f = g where g x = f x

twice :: (a -> a) -> (a -> a)
twice f = g where g x = f (f x)

doble :: Num a => a -> a
doble x = x + x

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

uflip :: ((b, a) -> c) -> ((a, b) -> c)
uflip f = g where g p = f (swap p)

-- Ejercicio 2
-- Dadas las definiciones anteriores, indicar el tipo de las siguientes expresiones:

-- apply first :: (a, b) -> a
-- first (swap, uflip) :: (a, b) -> (b, a)
-- twice doble :: Num a => a -> a
-- twice twice :: (a -> a) -> (a -> a)
-- twice uflip :: ((a, a) -> c) -> ((a, a) -> c)
-- twice swap :: (a, a) -> (a, a)
-- uflip swap :: (a, a) -> (a, a)
-- (twice twice) swap :: (a, a) -> (a, a)

-- Ejercicio 3
-- Dadas las siguientes definiciones y los siguientes tipos, asociar cada tipo con la función correspondiente.

const x = g where g y = x
const :: a -> (b -> a)

appDup f = g where g x = f (x, x)
appDup :: ((a, a) -> b) -> (a -> b)

appFork (f, g) = h where h x = (f x, g x)
appFork :: (a -> b, a -> c) -> (a -> (b, c))

appPar (f, g) = h where h (x, y) = (f x, g y)
appPar :: (a -> b, c -> d) -> ((a, c) -> (b, d))

appDist f = g where g (x, y) = (f x, f y)
appDist :: (a -> b) -> ((a, a) -> (b, b))

flip f = h where h x = k where k y = (f y) x
flip :: (a -> (b -> c)) -> (b -> (a -> c))

subst f = h where h g = k where k x = (f x) (g x)
subst :: (a -> (b -> c)) -> ((a -> b) -> (a -> c))

-- Ejercicio 4
-- Para cada una de las siguientes expresiones decidir si poseen tipo. Si es así indicar cuál es.

-- 1 && 2 == 2 :: No tiene tipo
-- 1 + if 3 < 5 then 3 else 5 :: Int
-- let par = (True, 4) in (if first par then first par else second par) :: No tiene tipo
-- (doble doble) 5 :: No tiene tipo
-- doble (doble 5) :: Int
-- twice first :: No tiene tipo
-- (twice doble) doble :: No tiene tipo
-- (twice twice) first :: No tiene tipo
-- apply apply :: (a -> b) -> (a -> b)

-- Ejercicio 6
-- Para cada una de las siguientes expresiones, decir a cuál función del ejercicio 3 es equivalente. Ofrecer argumentos de por qué son equivalentes.

-- \p -> let (f, g) = p in \x -> (f x, g x) :: appFork

-- \f -> (\g -> (\x -> f x (g x)) :: subst

-- \f -> (\x -> (\y -> (f y) x) :: flip

-- \f -> (\px -> let (x, y) = px in (f x, f y)) :: appDist

-- \x -> (\y -> x) :: const

-- \pf -> let (f, g) = pf in \px -> let (x, y) = px in (f x, g y) :: appPar

-- \f -> (\x -> f (x, x)) :: appDup

-- Ejercicio 7
-- Encontrar cuales de estas expresiones son equivalentes entre sí. Sugerencia: utilizar funciones anónimas es una forma interesante de encontrar equivalencias entre expresiones que denotan funciones.

-- appFork (id, id) :: a -> (a, a)
-- \f -> appDup (appDist f) :: (a -> b) -> a -> (b, b)
-- appDup id :: a -> (a, a)
-- appDup appFork :: (a -> b) -> a -> (b, b)
-- flip (appDup const) :: a -> b -> (b, b)
-- const (appDup id) :: a -> b -> (b, b)
