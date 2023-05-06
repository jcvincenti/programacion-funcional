data Fila a = Fin | Celda Int a (Fila a) deriving (Show, Eq)

ej = Celda 3 "casa" (Celda 8 "hola" (Celda 7 "casa" Fin))

many :: Int -> (a -> a) -> a -> a
many 0 f x = x
many n f x = f (many (n-1) f x)

-- f Fin = ...
-- f (Celda n x fila) = ... f fila

-- cuenta la cantidad de veces que aparece un elemento que cumple el predicado
countF :: (a -> Bool) -> Fila a -> Int
countF f Fin = 0
countF f (Celda n a fila) = if f a then n + countF f fila else countF f fila

-- le suma N a los elementos donde el predicado da verdadero
sumarN :: (a -> Bool) -> Int -> Fila a -> Fila a
sumarN f cant Fin = Fin
sumarN f cant (Celda n a fila) = let b = if f a then cant else 0 in Celda (n + b) a (sumarN f cant fila)

-- Junta dos filas manteniendo el orden de los elementos
concatenarF :: Fila a -> Fila a -> Fila a
concatenarF Fin fila = fila
concatenarF (Celda n a fila) fila2 = Celda n a (concatenarF fila fila2)

-- transforma cada elemento aplicando una función a los mismos
mapF :: (a -> b) -> Fila a -> Fila b
mapF f Fin = Fin
mapF f (Celda n a fila) = Celda n (f a) (mapF f fila)

-- transforma una fila de filas en una fila
aplanar :: Fila (Fila a) -> Fila a
aplanar Fin = Fin
aplanar (Celda n x fila) = many n (concatenarF x) (aplanar fila)

-- los elementos iguales los colapsa en una misma posición (sean contiguos o no)
comprimir :: Eq a => Fila a -> Fila a
comprimir Fin = Fin
comprimir (Celda n a fila) = Celda (n+countF (==a) fila) a (comprimir (eliminarDeFila a fila))

eliminarDeFila :: Eq a => a -> Fila a -> Fila a
eliminarDeFila a Fin = Fin
eliminarDeFila a (Celda x b fila2) = if a==b then eliminarDeFila a fila2 else Celda x b (eliminarDeFila a fila2)

agruparIguales :: Eq a => Fila a -> Fila a
agruparIguales Fin = Fin
agruparIguales (Celda n x fila) = agregarASuIgual n x (agruparIguales fila)

agregarASuIgual :: Eq a => Int -> a -> Fila a -> Fila a
agregarASuIgual n x Fin = Celda n x Fin
agregarASuIgual n x (Celda m y fila) =
    if x == y
        then Celda (n+m) x fila
        else Celda m y (agregarASuIgual n x fila)


-- denota la composición de las funciones manteniendo el orden de aparición y aplicandolas las veces que aparezca
componer :: Fila (a -> a) -> (a -> a)
componer Fin = id
componer (Celda n f fila) = many n f . componer fila
