-- Ejercicio 1

data EA = Const Int | BOp BinOp EA EA deriving (Show, Eq)
data BinOp = Sum | Mul deriving (Show, Eq)

data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA deriving (Show, Eq)

ej1 = BOp Sum (BOp Mul (Const 3) (Const 4)) (Const 4)

-- que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalEA :: EA -> Int
evalEA (Const num) = num
evalEA (BOp bop ea1 ea2) = doEvalEA bop (evalEA ea1) (evalEA ea2)

doEvalEA :: BinOp -> Int -> Int -> Int
doEvalEA Sum n1 n2 = n1 + n2
doEvalEA Mul n1 n2 = n1 * n2

-- que describe una expresión aritmética representada con el tipo ExpA, cuya estructura y significado 
-- son los mismos que la dada.
ea2ExpA :: EA -> ExpA
ea2ExpA (Const num) = Cte num
ea2ExpA (BOp bop ea1 ea2) = doEa2ExpA bop (ea2ExpA ea1) (ea2ExpA ea2)

doEa2ExpA :: BinOp -> ExpA -> ExpA -> ExpA
doEa2ExpA Sum ea1 ea2 = Suma ea1 ea2
doEa2ExpA Mul ea1 ea2 = Prod ea1 ea2

-- que describe una expresión aritmética representada con el tipo EA, cuya estructura y significado
-- son los mismos que la dada.
expA2ea :: ExpA -> EA
expA2ea (Cte num) = Const num
expA2ea (Suma ea1 ea2) = BOp Sum (expA2ea ea1) (expA2ea ea2)
expA2ea (Prod ea1 ea2) = BOp Mul (expA2ea ea1) (expA2ea ea2)


-- demostrar la siguiente propiedad:
-- ea2ExpA . expA2ea = id

-- expA2ea . ea2ExpA = id

-- evalExpA . ea2ExpA = evalEA

-- evalEA . expA2ea = evalExpA


-- Ejercicio 2

data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b) deriving (Show, Eq)

ej2 = Nodo 1 (Nodo 2 (Hoja 3) (Hoja 4)) (Nodo 5 (Hoja 6) (Nodo 7 (Hoja 8) (Hoja 9)))

-- que describe la cantidad de hojas en el árbol dado.
cantidadDeHojas :: Arbol a b -> Int
cantidadDeHojas (Hoja a) = 1
cantidadDeHojas (Nodo a ab1 ab2) = cantidadDeHojas ab1 + cantidadDeHojas ab2

-- que describe la cantidad de nodos en el árbol dado
cantidadDeNodos :: Arbol a b -> Int
cantidadDeNodos (Hoja a) = 0
cantidadDeNodos (Nodo a ab1 ab2) = 1 + cantidadDeNodos ab1 + cantidadDeNodos ab2

-- que describe la cantidad de constructores en el árbol dado
cantidadDeConstructores :: Arbol a b -> Int
cantidadDeConstructores (Hoja a) = 1
cantidadDeConstructores (Nodo a ab1 ab2) = 1 + cantidadDeConstructores ab1 + cantidadDeConstructores ab2

-- que describe la representación como elemento del tipo Arbol BinOp Int de la expresión aritmética dada.
ea2Arbol :: EA -> Arbol BinOp Int
ea2Arbol (Const num) = Hoja num
ea2Arbol (BOp bop ea1 ea2) = Nodo bop (ea2Arbol ea1) (ea2Arbol ea2)

-- Ejercicio 3

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
ej3 = NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT) (NodeT 4 EmptyT EmptyT)) (NodeT 5 (NodeT 6 EmptyT EmptyT) (NodeT 7 EmptyT EmptyT))
-- f EmptyT = ...
-- f (NodeT a ti td) = ... f ti ... f td

-- que describe el número resultante de sumar todos los números en el árbol dado.
sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT a ti td) = a + sumarT ti + sumarT td


-- que describe la cantidad de elementos en el árbol dado.
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT a ti td) = 1 + sizeT ti + sizeT td

-- que indica si en el árbol dado hay al menos un elemento que cumple con el predicado dado.
anyT :: (a -> Bool) -> Tree a -> Bool
anyT p EmptyT = False
anyT p (NodeT a ti td) = p a || anyT p ti || anyT p td

-- que describe la cantidad de elementos en el árbol dado que cumplen con el predicado dado.
countT :: (a -> Bool) -> Tree a -> Int
countT p EmptyT = 0
countT p (NodeT a ti td) = if p a then 1 else 0 + countT p ti + countT p td

-- que describe la cantidad de hojas del árbol dado.
countLeaves :: Tree a -> Int
countLeaves EmptyT = 0
countLeaves (NodeT a ti td) = if esHoja ti td then 1 else 0 + countLeaves ti + countLeaves td

esHoja :: Tree a -> Tree a -> Bool
esHoja EmptyT EmptyT = True
esHoja _ _ = False

-- que describe la altura del árbol dado.
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT a ti td) = 1 + heightT ti `max` heightT td

-- que describe la lista in order conlos elementos del árbol dado.
inOrder :: Tree a -> [a]
inOrder EmptyT = []
inOrder (NodeT a ti td) = inOrder ti ++ [a] ++ inOrder td

-- que describe la lista donde cada elemento es una lista con los elementos de un nivel del árbol dado.
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT a ti td) = [a] : juntarNiveles (listPerLevel ti) (listPerLevel td)

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles [] [] = []
juntarNiveles [] (y:ys) = y:ys
juntarNiveles (x:xs) [] = x:xs
juntarNiveles (x:xs) (y:ys) = (x ++ y) : juntarNiveles xs ys

-- que describe un árbol con los mismos elementos que el árbol dado pero en orden inverso.
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT a ti td) = NodeT a (mirrorT td) (mirrorT ti)

-- que describe la lista con los elementos del nivel dado en el árbol dado.
levelN :: Int -> Tree a -> [a]
levelN n EmptyT = []
levelN 0 (NodeT a ti td) = [a]
levelN n (NodeT a ti td) = levelN (n-1) ti ++ levelN (n-1) td

-- que describe la lista con los elementos de la rama más larga del árbol.
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT a ti td) = a : if heightT ti > heightT td then ramaMasLarga ti else ramaMasLarga td

-- que describe la lista con todos los caminos existentes en el árbol dado.
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT a EmptyT EmptyT) = [[a]]
todosLosCaminos (NodeT a ti td) = agregarRaiz a (todosLosCaminos ti ++ todosLosCaminos td)

agregarRaiz :: a -> [[a]] -> [[a]]
agregarRaiz a [] = []
agregarRaiz a (x:xs) = (a:x) : agregarRaiz a xs


-- Ejercicio 4

data AppList a = Single a | Append (AppList a) (AppList a)

-- f (Single a) = ...
-- f (Append ali ald) = ... f ali ... f ald

-- que describe la cantidad de elementos de la lista.
lenAL :: AppList a -> Int
lenAL (Single a) = 1
lenAL (Append ali ald) = lenAL ali + lenAL ald

-- que describe la lista resultante de agregar el elemento dado al principio de la lista dada.
consAL :: a -> AppList a -> AppList a
consAL e (Single a) = Append (Single e) (Single a)
consAL e (Append ali ald) = Append (Single e) (Append ali ald) -- ????

-- que describe el primer elemento de la lista dada. 
headAL :: AppList a -> a
headAL (Single a) = a
headAL (Append ali ald) = headAL ali

-- que describe la lista resultante de quitar el primer elemento de la lista dada. 
tailAL :: AppList a -> AppList a
tailAL (Single a) = error "No se puede sacar el elemento"
tailAL (Append (Single a) ald) = ald
tailAL (Append ali ald) = Append (tailAL ali) ald

-- que describe la lista resultante de agregar el elemento dado al final de la lista dada. 
snocAL :: a -> AppList a -> AppList a
snocAL e (Single a) = Append (Single a) (Single e)
snocAL e (Append ali ald) = Append ali (snocAL e ald)

-- que describe el último elemento de la lista dada. 
-- lastAL :: AppList a -> a

-- que describe la lista dada sin su último elemento. 
-- initAL :: AppList a -> AppList a

-- que describe la lista dada con sus elementos en orden inverso. 
-- reverseAL :: AppList a -> AppList a

-- que indica si el elemento dado se encuentra en la lista dada. 
-- elemAL :: Eqa=>a -> AppList a -> Bool

-- que describe el resultado de agregar los elementos de la primera lista adelante de los elementos de la segunda.
-- NOTA: buscar la manera más eficiente de hacerlo.
-- appendAL :: AppList a -> AppList a -> AppList a

-- que describe la representación lineal de la lista dada.
-- appListToList :: AppList a -> [a]


-- Ejercicio 5

data QuadTree a = LeafQ a | NodeQ (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)
data Color = RGB Int Int Int
type Image = QuadTree Color

-- f (LeafQ a) = ...
-- f (NodeQ qt1 qt2 qt3 qt4) = ... f qt1 ... f qt2 ... f qt3 ... f qt4

-- que describe la altura del árbol dado.
heightQT :: QuadTree a -> Int
heightQT (LeafQ a) = 1
heightQT (NodeQ qt1 qt2 qt3 qt4) = 1 + heightQT qt1 `max` heightQT qt2 `max` heightQT qt3 `max` heightQT qt4

-- que describe la cantidad de hojas del árbol dado. 
countLeavesQT :: QuadTree a -> Int
countLeavesQT (LeafQ a) = 1
countLeavesQT (NodeQ qt1 qt2 qt3 qt4) = countLeavesQT qt1 + countLeavesQT qt2 + countLeavesQT qt3 + countLeavesQT qt4

-- que describe la cantidad de constructores del árbol dado. 
sizeQT :: QuadTree a -> Int
sizeQT (LeafQ a) = 1
sizeQT (NodeQ qt1 qt2 qt3 qt4) = 1 + sizeQT qt1 + sizeQT qt2 + sizeQT qt3 + sizeQT qt4

-- que describe el árbol resultante de transformar en hoja todos aquellos nodos para los que se cumpla que todos los elementos 
-- de sus subárboles son iguales.
compress :: QuadTree a -> QuadTree a
compress (LeafQ a) = LeafQ a
compress (NodeQ qt1 qt2 qt3 qt4) = if qt1 `equalQT` qt2 `equalQT` qt3 `equalQT` qt4
    then LeafQ (elemQT qt1)
    else NodeQ (compress qt1) (compress qt2) (compress qt3) (compress qt4)

-- que describe el árbol resultante de transformar en nodo (manteniendo el dato de la hoja correspondiente)
-- todas aquellas hojas que no se encuentren en el nivel de la altura del árbol. 
uncompress :: QuadTree a -> QuadTree a

-- que describela imagen dada en el tamaño dado. Precondición: el tamaño dado es potencia de 4 y su raíz cuarta es mayor o igual
-- a la altura del árbol dado.
-- NOTA: Una imagen tiene tamaño t cuando todas las hojas se encuentran en el nivel ∜t.
-- AYUDA: Esta operación es similar a un compress, pero pudiendo variar la altura del árbol
render :: Image -> Int -> Image