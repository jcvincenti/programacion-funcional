# Práctica 4

## Ejercicio 1
Determinar si las siguientes funciones son parciales o totales. Justificar. 

- `udiv (x,y) = div x y` 
- ```
    udivE (x,0) = error "No puedo dividir por 0"
    udivE (x,y) = div x y
- `udivH = uncurry div`
- ``succ x = x + 1``
- ``succH = suma 1``
- ``porLaMitad = flip div 2``
- ``conDieresis 'u' = 'ü'``
- ```
    conDieresisB 'u' = 'ü'
    conDieresisB c = conDieresisB c
- ```
    conTildePM 'a' = 'á'
    conTildePM 'e' = 'é'
    conTildePM 'i' = 'í'
    conTildePM 'o' = 'ó'
    conTildePM 'u' = 'ú'
- ```
    conTildeE c = if esVocal c
        then conTildePM c
        else error "El valor recibido no es vocal"
- ```
    conTilde c = if esVocal c && esMinuscula c
        then conTildePM c
        else c 

## Ejercicio 2
Para cada una de las funciones del ejercicio anterior, determinar si una o más de las otras es equivalente a ella.

## Ejercicio 3
Dada la siguiente definición para la función twice:

`twice = \f -> \x -> f (f x)`

Determinar cuántos y cuáles son los redexes en las siguientes expresiones.
- `twice doble`
- ``twice doble 2``
- ``twice``

## Ejercicio 4
Dada la siguiente definición para la función twice:

`twice f = g where g x = f (f x)`

Determinar cuántos y cuáles son los redexes en las siguientes expresiones.
- `twice doble`
- ``twice doble 2``
- ``twice``

## Ejercicio 5
Dada la siguiente definición para la función twice:

`twice f x = f (f x)`

Determinar cuántos y cuáles son los redexes en las siguientes expresiones.
- `twice doble`
- ``twice doble 2``
- ``twice``

## Ejercicio 6
Para cada tipo a continuación, intentar dar dos expresiones que denoten valores diferentes. Las expresiones deben ser diferentes de bottom, y en el caso de ser funciones, una debe ser total y otra debe ser parcial. De no ser posible hacer alguno de los casos, explicar por qué.

- `a`
- `Int -> a`
- `a -> b`
