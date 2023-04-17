# Práctica 2

## Ejercicio 1
Indicar los tipos de las siguientes definiciones:

- `first (x,y) = x`
- `apply f = g where g x = f x`

- `twice f = g where g x = f (f x)`
- `doble x = x + x`
- `swap (x, y) = (y, x)`
- `uflip f = g where g p = f (swap p)`

## Ejercicio 2
Dadas las definiciones anteriores, indicar el tipo de las siguientes expresiones:
- `apply first`
- `first (swap, uflip)`
- `twice doble`
- `twice twice`
- `twice uflip`
- `twice swap`
- `uflip swap`
- `(twice twice) swap`

## Ejercicio 3
Dadas las siguientes definiciones y los siguientes tipos, asociar cada tipo con la función correspondiente.
1. `const x = g where g y = x`
2. `appDup f = g where g x = f (x, x)`
3. `appFork (f, g) = h where h x = (f x, g x)`
4. `appPar (f, g) = h where h (x, y) = (f x, g y)`
5. `appDist f = g where g (x, y) = (f x, f y)`
6. `flip f = h where h x = k where k y = (f y) x`
7. `subst f = h where h g = k where k x = (f x) (g x)`


- `(a -> b, c -> d) -> ((a, c) -> (b, d))`
- `((a, a) -> b) -> (a -> b)`
- `(a -> (b -> c)) -> (b -> (a -> c))`
- `(a -> b) -> ((a, a) -> (b, b))`
- `a -> b, a -> c) -> (a -> (b, c))`
- `(a -> (b -> c)) -> ((a -> b) -> (a -> c))`
- `a -> (b -> a)`

## Ejercicio 4
Para cada una de las siguientes expresiones decidir si poseen tipo. Si es así indicar cuál es.

- `1 && 2 == 2`
- `1 + if 3 < 5 then 3 else 5`
- `let par = (True, 4) in (if first par then first par else second par)`
- `(doble doble) 5`
- `doble (doble 5)`
- `twice first`
- `(twice doble) doble`
- `(twice twice) first`
- `apply apply`

## Ejercicio 5
Dar dos ejemplos de expresiones que tengan cada uno de los siguientes tipos:

- `Bool`
- `(Int, Int)`
- `Char -> Int`
- `(Int, Char) -> Bool`
- `(Int -> Int) -> Int`
- `(Bool -> Bool, Int)`
- `a -> Bool`

## Ejercicio 6
Para cada una de las siguientes expresiones, decir a cuál función del ejercicio 3 es equivalente. Ofrecer argumentos de por qué son equivalentes.

- `\p -> let (f, g) = p in \x -> (f x, g x)`
- `\f -> (\g -> (\x -> f x (g x))`
- `\f -> (\x -> (\y -> (f y) x)`
- `\f -> (\px -> let (x, y) = px in (f x, f y))`
- `\x -> (\y -> x)`
- `\pf -> let (f, g) = pf in \px -> let (x, y) = px in (f x, g y)`
- `\f -> (\x -> f (x, x))`

## Ejercicio 7
Encontrar cuales de estas expresiones son equivalentes entre sí. Sugerencia: utilizar funciones anónimas es una forma interesante de encontrar equivalencias entre expresiones que denotan funciones.

- `appFork (id,id)`
- `\f -> appDup (appDist f)`
- `appDup id`
- `appDup appFork`
- `flip (appDup const)`
- `const (appDup id)`
