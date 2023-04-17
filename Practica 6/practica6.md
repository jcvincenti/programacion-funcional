# Práctica 6

## Ejercicio 1
Demostrar las siguientes propiedades:
- `doble = \x -> 2 * x`
- `compose doble doble = cuadruple`

## Ejercicio 2
Demostrar las siguientes propiedades:
- para todo x. para todo y. `x && y = not ((not x) || (not y))`
- para todo x. para todo y. `not (x || y) = not x && not y`

## Ejercicio 3
Demostrar las siguientes propiedades:
- `curry suma' = suma`
- `uncurry suma = suma'`

## Ejercicio 4
Demostrar las siguientes propiedades:
- `curry fst = const`
- `uncurry (flip const) = snd`

## Ejercicio 5
Demostrar las siguientes propiedades:
- para todo f. `curry (uncurry f) = f`
- para todo f'. `uncurry (curry f') = f'`

## Ejercicio 6
Dadas las siguientes definiciones
`assoc :: (a,(b,c)) -> ((a,b),c)`

`assoc (x,(y,z)) = ((x,y),z)`

`appAssoc :: (((a,b),c) -> d) -> (a,(b,c)) -> d`

`appAssoc f p = f (assoc p)`

Demostrar la siguiente propiedad:
para todo f.

`appAssoc (uncurry (uncurry f)) = uncurry (compose uncurry f)`

**AYUDA**: De necesitar el principio de extensionalidad, considerar usar el argumento en la forma (x,(y,z)).

## Ejercicio 7
Dada la siguiente definición

`(f . g) x = f (g x)`

- Definir las siguientes funciones utilizando el operador (.) y la menor cantidad de parámetros posible:
  - cuadruple
  - doble
  - twice
  - many
**Ayuda**: pueden utilizarse como definidas las funciones succ, doble, const, id, subst, dup, etc.
- Demostrar las siguientes propiedades:
    - para todo f. para todo g. f . g = compose f g
    - swap . swap = id
    - para todo f. para todo g. para todo h. `f . (g . h) = (f . g) . h`
    - curry . uncurry = id
    - para todo f. appAssoc f = f . assoc
- Demostrar las siguientes propiedades solamente mediante otras propiedades ya demostradas (sin utilizar el principio de extensionalidad ni las definiciones de las funciones directamente):
    - `doble . doble = cuadruple`
    - para todo f'. `curry (uncurry (curry f')) = curry f'`
    - para todo f. `appAssoc (uncurry (uncurry f)) = (uncurry . uncurry) f . assoc`
    - para todo f. `(uncurry . uncurry) f . assoc = uncurry (uncurry . f)`
