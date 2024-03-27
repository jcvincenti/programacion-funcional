# Práctica 3

## Ejercicio 1
Definir funciones:
1. `curry :: ((a,b) -> c) -> a -> b -> c`
2. `uncurry :: (a -> b -> c) -> (a,b) -> c`


de tal manera que:


- Para toda función `f :: a -> b -> c` , se cumple `curry (uncurry f) = f`
- Para toda función `f’ :: (a,b) -> c` , se cumple `uncurry (curry f') = f'`

## Ejercicio 2
Reescribir las siguientes definiciones sin utilizar where , let o lambdas, y utilizando la menor cantidad de paréntesis posible.

- `apply f = g where g x = f x`
- `twice f = g where g x = f (f x)`
- `id = \x -> x`
- `flip f = g where g x = h where h y = (f y) x`
- `uflip f = g where g p = f (swap p)`
- `const = \x -> (\y -> x)`
- `compose = \f -> (\g -> (\x -> f (g x)))`

## Ejercicio 3
Indicar el tipo de cada una de las funciones del ejercicio anterior, utilizando también la menor cantidad posible de paréntesis.


## Ejercicio 4
En las expresiones que siguen, colocar los paréntesis que están implícitos, manteniendo el significado de cada una de las expresiones, y dar el tipo de cada una de ellas, suponiendo dadas las definiciones de los ejercicios anteriores.

- ``apply apply apply``
- ``twice doble 2``
- ``twice twice twice swap``
- ``flip twice 1 doble``

## Ejercicio 5
Reescribir las siguientes definiciones utilizando sólo lambdas (sin where ni let ).

- ``appDup f = g where g x = f (x, x)``
- ``appFork (f, g) = h where h x = (f x, g x)``
- ``appPar (f, g) = h where h (x, y) = (f x, g y)``
- ``appDist f = g where g (x, y) = (f x, f y)``
- ``subst f = h where h g = k where k x = (f x) (g x)``

## Ejercicio 6
Indicar cuáles de las siguientes expresiones tienen tipo según el sistema de tipos de Hindley Milner. En el caso de que alguna sea incorrecta, ¿existirá una expresión que utilice las mismas partes, pero asociadas de forma diferente y que sí tenga significado? En el caso de que sí, escribir tal variante.

- ``compose (fst snd)``
- ``(uncurry curry snd)``
- ``(apply id) ((id apply) apply)``
- ``compose (compose doble doble)``
- ``(compose compose) doble doble``

## Ejercicio 7
Dada la siguiente definición, indicar cómo podría reescribirse usando compose y id :

- ``many :: Int -> (a -> a) -> a -> a``
- ``many 0 f x = x``
- ``many n f x = f (many (n-1) f x)``

## Ejercicio 8
Quitar de los siguientes tipos la mayor cantidad de paréntesis posible, sin cambiar su significado. En cada caso, escribir en castellano cómo debería leerse el tipo obtenido de forma correcta, y cómo con la convención de leerla como si estuviera no-currificada.


Por ejemplo, para Int -> (Int -> Int) las respuestas serían: Int -> Int -> Int, en castellano en forma correcta “es una función que toma un entero y devuelve una función que toma otro entero y devuelve un entero” , y en castellano usando la convención “ es una función que toma dos enteros y devuelve un entero” .

- ``(Int -> Int) -> (Int -> Int)``
- ``(a -> (b -> c)) -> (a -> b) -> c``
- ``(a -> b, c -> d) -> ((a, c) -> (b, d))``
- ``((a, a) -> b) -> (a -> b)``
- ``(a -> (b -> c)) -> (b -> (a -> c))``
- ``(a -> b) -> ((a, a) -> (b, b))``
- ``( a -> b, a -> c) -> (a -> (b, c))``
- ``(a -> (b -> c)) -> ((a -> b) -> (a -> c))``
- ``a -> (b -> a)``

## Ejercicio 9
Dar expresiones equivalentes a las funciones definidas a continuación utilizando funciones como compose , flip , etc. (dadas en los ejercicios anteriores) y sin utilizar lambas.

- ``cuadruple x = doble (doble x)``
- ``timesTwoPlusThree x = suma (doble x) 3``
- ``fourTimes f x = f (f (f (f x)))``

## Ejercicio 10
Investigar la notación de sección de operadores.
