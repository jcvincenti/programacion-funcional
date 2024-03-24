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
foldT e n (NodeT a t1 t2) = n a (foldT e n t1) (foldT e n t2)

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
partitionT f = foldT ([], []) (\a (xs, ys) (xss, yss) -> if f a then (a:xs ++ xss, ys ++ yss) else (xs ++ xss, a:ys ++ yss))

-- zipWithT :: (a->b->c) -> Tree a -> Tree b -> Tree c
caminoMasLargo :: Tree a -> [a]
caminoMasLargo = foldT [] (\a xs ys -> if length xs > length ys then a:xs else a:ys)

-- todosLosCaminos :: Tree a -> [[a]]
-- todosLosNiveles :: Tree a -> [[a]]
-- nivelN :: Tree a -> Int -> [a]

-- Ejercicio 4
type Record a b = [(a,b)]
-- Donde la idea de este tipo es representar una fila de una base de datos
-- (el valor de tipo a es el nombre del campo, y el valor de tipo b es el valor de ese campo).
-- Con esto, puede definirse el tipo:
type Table a b = [ Record a b ]
-- Donde se entiende que una tabla de una base de datos está compuesta por muchos registros, y se
-- espera que todos compartan los mismos “campos” (osea, los valores de tipo a en cada registro).

select :: (Record a b -> Bool) -> Table a b -> Table a b
select f = filter f

project :: (a -> Bool) -> Table a b -> Table a b
project f = map (doProject f)

doProject :: (a -> Bool) -> Record a b -> Record a b
doProject f = foldr (\(a, b) kvs -> if f a then (a, b):kvs else kvs) []

conjunct :: (a -> Bool) -> (a -> Bool) -> a -> Bool
conjunct f1 f2 a = f1 a && f2 a

-- Describe la lista de registros resultante del producto cartesiano combinado de las dos listas de registros dadas.
-- Es decir, la unión de los campos en los registros del producto cartesiano.
product :: Table a b -> Table a b -> Table a b

similar :: Record a b -> Record a b

-- Describe el resultado de aplicar una funcion a cada elemento del producto cartesiano de las
-- dos listas de registros dadas
crossWith :: (a -> b -> c) -> [a] -> [b] -> [c]
crossWith f xs ys = [f x y | x <- xs, y <- ys]

-- Ejercicio 5
data Query a b = Table [Record a b]           -- Table (Table a b)
    | Product (Query a b) (Query a b)
    | Projection (a -> Bool) (Query a b)
    | Selection (Record a b -> Bool) (Query a b)

foldQ :: ([Record a b] -> t) -> (t -> t -> t) -> ((a -> Bool) -> t -> t) -> ((Record a b -> Bool) -> t -> t) -> Query a b -> t
foldQ t p pr s (Table rs) = t rs
foldQ t p pr s (Product qa qb) = p (foldQ t p pr s qa) (foldQ t p pr s qb)
foldQ t p pr s (Projection f qa) = pr f (foldQ t p pr s qa)
foldQ t p pr s (Selection f qa) = s f (foldQ t p pr s qa)

-- Describe la lista de todas las tablas involucradas en la query dada.
tables :: Query a b -> [Table a b]
tables = foldQ (\rs -> [rs]) (++) (\f rs -> rs) (\f rs -> rs)

execute :: Query a b -> Table a b
execute = foldQ id product project select

-- Ejercicio 6

data Dir = DirLeft | DirRight | DirStraight
data Mapa a = Cofre [a] | Nada (Mapa a) | Bifurcacion [a] (Mapa a) (Mapa a)

foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM c n b (Cofre xs) = c xs
foldM c n b (Nada m) = n (foldM c n b m)
foldM c n b (Bifurcacion xs m1 m2) = b xs (foldM c n b m1) (foldM c n b m2)

objects :: Mapa a -> [a]
objects = foldM id id (\xs m1 m2 -> xs ++ m1 ++ m2)

mapM :: (a -> b) -> Mapa a -> Mapa b
mapM f = foldM (\xs -> Cofre (map f xs)) Nada (\xs m1 m2 -> Bifurcacion (map f xs) m1 m2)

has :: (a -> Bool) -> Mapa a -> Bool
has f = foldM (any f) id (\xs m1 m2 -> any f xs || m1 || m2)

longestPath :: Mapa a -> [Dir]
longestPath = foldM (const []) (\m -> DirStraight : m) (\xs m1 m2 -> if length m1 > length m2 then DirLeft:m1 else DirRight:m2)

objectsOfLongestPath :: Mapa a -> [[a]]
objectsOfLongestPath = foldM (\xs -> [xs]) id (\xs om1 om2 -> if length om1 > length om2 then xs:om1 else xs:om2)

