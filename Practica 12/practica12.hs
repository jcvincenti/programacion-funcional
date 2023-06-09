-- Ejercicio 1
data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA

foldExpa c s p (Cte n) = c n
foldExpa c s p (Suma e1 e2) = s (foldExpa c s p e1) (foldExpa c s p e2)
foldExpa c s p (Prod e1 e2) = p (foldExpa c s p e1) (foldExpa c s p e2)

-- que describe la cantidad de ceros explícitos en la expresión dada.
cantidadDeCeros :: ExpA -> Int
cantidadDeCeros = foldExpa (\n -> if n == 0 then 1 else 0) (+) (+)

-- que describe si la expresión dada no tiene números negativos de manera explícita.
noTieneNegativosExplicitosExpA :: ExpA -> Bool
noTieneNegativosExplicitosExpA = foldExpa (\n -> n > -1) (&&) (&&)

-- que describe una expresión con el mismo significado que la dada,
-- pero que no tiene sumas del número 0 ni multiplicaciones por 1 o por 0. La resolución debe ser exclusivamente simbólica.
simplificarExpA' :: ExpA -> ExpA
simplificarExpA' = foldExpa (Cte) (simplificarSuma) (simplificarProd)

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Cte 0) e2 = e2
simplificarSuma e1 (Cte 0) = e1
simplificarSuma e1 e2 = Suma e1 e2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Cte 0) e2 = Cte 0
simplificarProd e1 (Cte 0) = Cte 0
simplificarProd (Cte 1) e2 = e2
simplificarProd e1 (Cte 1) = e1
simplificarProd e1 e2 = Prod e1 e2

-- que describe el número que resulta de evaluar la cuenta representada por la expresión aritmética dada.
evalExpA' :: ExpA -> Int
evalExpA' = foldExpa (id) (+) (*)

-- que describe el string sin espacios y con paréntesis correspondiente a la expresión dada.
showExpA :: ExpA -> String
showExpA = foldExpa (show) (\e1 e2 -> "(" ++ e1 ++ "+" ++ e2 ++ ")") (\e1 e2 -> "(" ++ e1 ++ "*" ++ e2 ++ ")")

-- que describe la cantidad de constructores de suma con al menos uno de sus hijos constante cero.
cantDeSumaCeros :: ExpA -> Int
cantDeSumaCeros = foldExpa (\n -> if n == 0 then 1 else 0) (+) (+) -- ????


-- Ejercicio 3
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

foldT e n (EmptyT) = e
foldT e n (Node a t1 t2) = n a (foldT e n t1) (foldT e n t2)

mapT :: (a -> b) -> Tree a -> Tree b
mapT f = foldT EmptyT (\a t1 t2 -> NodeT (f a) t1 t2)

sumT :: Tree Int -> Int
sumT = foldT 0 (\a t1 t2 -> a + t1 + t2)

sizeT :: Tree a -> Int
sizeT = foldT 0 (\a t1 t2 -> 1 + t1 + t2)

heightT :: Tree a -> Int
heightT = foldT 0 (\a t1 t2 -> 1 + t1 `max` t2)

preOrder :: Tree a -> [a]
preOrder = foldT [] (\a t1 t2 -> a : t1 ++ t2)

inOrder :: Tree a -> [a]
inOrder = foldT [] (\a t1 t2 -> t1 ++ [a] ++ t2)

postOrder :: Tree a -> [a]
postOrder = foldT [] (\a t1 t2 -> t1 ++ t2 ++ [a])

mirrorT :: Tree a -> Tree a
mirrorT = foldT EmptyT (\a t1 t2 -> NodeT a t2 t1)

countByT :: (a -> Bool) -> Tree a -> Int
countByT f = foldT 0 (\a t1 t2 -> (if f a then 1 else 0) + t1 + t2)

partitionT :: (a -> Bool) -> Tree a -> ([a], [a])
partitionT f = foldT ([], []) (\a (xs, ys) -> if f a then (a:xs, ys) else (xs, a:ys))

zipWithT :: (a->b->c) -> Tree a -> Tree b -> Tree c
caminoMasLargo :: Tree a -> [a]
caminoMasLargo = foldT [] (\a xs ys -> if length t1 > length t2 then a:xs else a:ys)

todosLosCaminos :: Tree a -> [[a]]
todosLosNiveles :: Tree a -> [[a]]
nivelN :: Tree a -> Int -> [a]
