doble x = x + x

cuadruple x = 4*x

-- twice f = g
--     where g x = f (f x)

-- twice f x = f (f x)

id' x = x

suma x = g
    where g y = x + y

const' x = g
    where g y = x

compose f = h
    where h g = k
            where k x = f(g x)

subst f = h
    where h g = k
            where k x = (f x) (g x)

-- Ejercicio 1
cuatro = \x->4
cuatro1 = \x -> id' 4
cuatro2 = doble (doble 1)
cuatro3 = doble (id' 2)
cuatro4 = doble ((const' 2) 5)
cuatro5 = cuadruple 1

-- Ejercicio 4
triple x = 3*x
succ' x = x + 1
sumarDos x = x + 2

-- Ejercicio 8
tripleL = \x -> 3*x
succL = \x -> x+1
sumarDosL = \x -> x + 2
twiceL = \f -> \x -> f(f x)

-- Ejercicio 9
-------------------------------
f x = let (y,z) = (x,x) in y
f1 x = x
-------------------------------
f' (x,y) = let z = x + y in g (z,y) where g (a,b) = a - b
-- f' (1, 2) = let z = 3 in g (3, 2) where g (3, 2) = 3 - 2
f1' (x, y) = x + y - y
-- f1' (x, y) = x
-------------------------------
f'' p = case p of (x,y) -> x
f1'' (x, y) = x
-------------------------------
f''' = \p -> let (x,y) = p in y
f1''' (x, y) = y