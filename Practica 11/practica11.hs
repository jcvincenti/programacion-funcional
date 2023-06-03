-- Ejercicio 1

-- Definir las siguientes funciones utilizando recursión estructural explícita sobre Pizza
data Pizza = Prepizza | Capa Ingrediente Pizza deriving (Show, Eq)
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamón | Queso | Salsa | MezclaRara deriving (Show, Eq)

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

cantidadCapasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen _ Prepizza = 0
cantidadCapasQueCumplen f (Capa i p) = if f i then 1 + cantidadCapasQueCumplen f p else cantidadCapasQueCumplen f p

conCapasTransformadas :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas _ Prepizza = Prepizza
conCapasTransformadas f (Capa i p) = Capa (f i) (conCapasTransformadas f p)

soloLasCapasQue :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue _ Prepizza = Prepizza
soloLasCapasQue f (Capa i p) = if f i then Capa i (soloLasCapasQue f p) else soloLasCapasQue f p

-- Ejercicio 2
-- Definir las siguientes funciones utilizando alguna de las definiciones anteriores:

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _ = False

sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza = Prepizza
sinLactosa (Capa i p) = soloLasCapasQue (\i -> not (esQueso i)) p

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza = True
aptaIntolerantesLactosa p = cantidadDeQueso p === 0

cantidadDeQueso :: Pizza -> Int
cantidadDeQueso Prepizza = True
cantidadDeQueso p = cantidadCapasQueCumplen esQueso p

doblarAceitunas :: Ingrediente -> Ingrediente
doblarAceitunas (Aceitunas n) = Aceitunas (n*2)
doblarAceitunas i = i

conElDobleDeAceitunas :: Pizza -> Pizza
conElDobleDeAceitunas Prepizza = Prepizza
conElDobleDeAceitunas (Capa i p) = Capa (doblarAceitunas i) (conElDobleDeAceitunas p)

-- Ejercicio 3
-- Definir pizzaProcesada, que expresa la definición de fold para la estructura de Pizza.

pizzaProcesada :: (Ingrediente -> b -> b) -> b -> Pizza -> b
pizzaProcesada fcap pr Prepizza = pr
pizzaProcesada fcap pr (Capa i p) = fcap i (pizzaProcesada fcap pr p)

-- Ejercicio 4
-- Resolver todas las funciones de los puntos 1) y 2) utilizando la función pizzaProcesada.

cantidadCapasQueCumplen' :: (Ingrediente -> Bool) -> Pizza -> Int
cantidadCapasQueCumplen' f = pizzaProcesada (\i pp -> unoSi (f i) + pp) 0

conCapasTransformadas' :: (Ingrediente -> Ingrediente) -> Pizza -> Pizza
conCapasTransformadas' f = pizzaProcesada (\i pp -> Capa (f i) pp) Prepizza

soloLasCapasQue' :: (Ingrediente -> Bool) -> Pizza -> Pizza
soloLasCapasQue' f = pizzaProcesada (\i pp -> if f i then Capa i pp else pp) Prepizza

sinLactosa' :: Pizza -> Pizza
sinLactosa' = pizzaProcesada (\i pp -> soloLasCapasQue (not . esQueso) pp) Prepizza

aptaIntolerantesLactosa' :: Pizza -> Bool
aptaIntolerantesLactosa' = pizzaProcesada (\i pp -> not (esQueso i) && pp) True

cantidadDeQueso' :: Pizza -> Int
cantidadDeQueso' = pizzaProcesada (\i pp -> if esQueso i then 1 + pp else pp) 0

conElDobleDeAceitunas' :: Pizza -> Pizza
conElDobleDeAceitunas' = pizzaProcesada (\i pp -> Capa (doblarAceitunas i) pp) Prepizza

-- Ejercicio 5
cantidadAceitunas :: Pizza -> Int
cantidadAceitunas = pizzaProcesada (\i pp -> doCantAceitunas i + pp) 0

doCantAceitunas :: Ingrediente -> Int
doCantAceitunas (Aceitunas n) = n
doCantAceitunas _ = 0

capasQueCumplen :: (Ingrediente -> Bool) -> Pizza -> [Ingrediente]
-- capasQueCumplen f Prepizza = []
-- capasQueCumplen f (Capa i p) = if f i then i : capasQueCumplen f p else capasQueCumplen f p
capasQueCumplen f = pizzaProcesada (\i pp -> if f i then i : pp else pp) []

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada = pizzaProcesada (\i pp -> juntarAceitunas i pp) Prepizza

juntarAceitunas :: Ingrediente -> Pizza -> Pizza
juntarAceitunas (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n+m)) p
juntarAceitunas i p = Capa i p

conCapasDe :: Pizza -> Pizza -> Pizza -- que agrega las capas de la primera pizza sobre la segunda
-- conCapasDe Preppiza p = p
-- conCapasDe (Capa ing p1) p2 = Capa ing (conCapasDe p1 p2)
conCapasDe p1 p2 = pizzaProcesada (\i pp -> Capa i pp) p2 p1

-- primerasNCapas :: Int -> Pizza -> Pizza

primerasNCapas 0 _ = Prepizza
primerasNCapas n Prepizza = Prepizza
primerasNCapas n (Capa i p) = Capa i (primerasNCapas (n-1) p)

-- Ejercicio 6
-- Demostraciones

-- Prop
-- para todo f. ¿ length . capasQueCumplen f = cantidadCapasQueCumplen f ?
-- Por ppio de ext. para todo p. ¿ length . capasQueCumplen f p = cantidadCapasQueCumplen f p?
-- Sea p una Pizza
-- Por def de (.)
-- ¿ length (capasQueCumplen f p) = cantidadCapasQueCumplen f p ?
-- Por ppio de ind. estr. sobre p

-- Caso base: p = Prepizza
-- length (capasQueCumplen f Prepizza) = cantidadCapasQueCumplen f Preppiza ?

-- Caso inductivo: p = Capa ing pz
-- HI: length (capasQueCumplen f pz) = cantidadCapasQueCumplen f pz
-- TI: length (capasQueCumplen f (Capa ing pz)) = cantidadCapasQueCumplen f (Capa ing pz)

-- Desarrollo

-- Caso base
-- lado izq

-- length (capasQueCumplen f Prepizza)
--                                      (capasQueCumple)
-- length []
--                                      (length)
-- 0

-- lado der
-- cantidadCapasQueCumplen f Preppiza
--                                      (cantidadCapasQueCumplen)
-- 0

-- Caso inductivo
-- lado izq

-- length (capasQueCumplen f (Capa ing pz))
--                                              (capasQueCumplen)
-- length (if f i then i : capasQueCumplen f pz else capasQueCumplen f pz)
-- Caso f i = True
-- length (i : capasQueCumplen f pz)
--                                      (prop practica 8)
-- length [i] + length (capasQueCumplen f pz)
--                                              (length)
-- 1 + length (capasQueCumplen f pz)
--                                              (HI)
-- 1 + cantidadCapasQueCumplen f pz

-- Caso f i = False
-- length (capasQueCumplen f pz)
--                                              (HI)
-- cantidadCapasQueCumplen f pz


-- lado der
-- cantidadCapasQueCumplen f (Capa ing pz)
--
-- if f i then 1 + cantidadCapasQueCumplen f pz else cantidadCapasQueCumplen f pz
-- Caso f i = True
-- 1 + cantidadCapasQueCumplen f pz

-- Caso f i = False
-- cantidadCapasQueCumplen f pz
-----------------------------------------------------------------------------------------------------------------------
-- Prop
-- para todo f. para todo p1. para todo p2.
-- ¿ cantidadCapasQueCumplen f (conCapasDe p1 p2) = cantidadCapasQueCumplen f p1 + cantidadCapasQueCumplen f p2 ?
-- Por ppio de ind. estr. sobre p1

-- Caso base: p1 = Prepizza
-- cantidadCapasQueCumplen f (conCapasDe Preppiza p2) = cantidadCapasQueCumplen f Prepizza + cantidadCapasQueCumplen f p2

-- Caso inductivo: p1 = Capa ing pz
-- HI: cantidadCapasQueCumplen f (conCapasDe pz p2) = cantidadCapasQueCumplen f pz + cantidadCapasQueCumplen f p2
-- TI: ¿ cantidadCapasQueCumplen f (conCapasDe (Capa ing pz) p2) = cantidadCapasQueCumplen f (Capa ing pz) + cantidadCapasQueCumplen f p2 ?

-- Desarrollo
-- Caso base
-- lado izq

-- cantidadCapasQueCumplen f (conCapasDe Preppiza p2)
--                                                      (conCapasDe)
-- cantidadCapasQueCumplen f p2

-- lado der
-- cantidadCapasQueCumplen f Prepizza + cantidadCapasQueCumplen f p2
--                                                                      (cantidadCapasQueCumplen)
-- 0 + cantidadCapasQueCumplen f p2
--                                      (aritm.)
-- cantidadCapasQueCumplen f p2

-- Caso inductivo
-- lado izq

-- cantidadCapasQueCumplen f (conCapasDe (Capa ing pz) p2)
--                                                                                                                  (conCapasDe)
-- cantidadCapasQueCumplen f (Capa ing (conCapasDe pz p2))
--                                                                                                                  (cantidadCapasQueCumplen)
-- if f i then 1 + cantidadCapasQueCumplen f (conCapasDe pz p2) else cantidadCapasQueCumplen f (conCapasDe pz p2)

-- Caso f i = True
-- 1 + cantidadCapasQueCumplen f (conCapasDe pz p2)
--                                                                                                                  (HI)
-- 1 + cantidadCapasQueCumplen f pz + cantidadCapasQueCumplen f p2

-- Caso f i = False
-- cantidadCapasQueCumplen f (conCapasDe pz p2)
--                                                                                                                  (HI)
-- cantidadCapasQueCumplen f pz + cantidadCapasQueCumplen f p2

-- lado der
-- cantidadCapasQueCumplen f (Capa ing pz) + cantidadCapasQueCumplen f p2
--                                                                                                                  (cantidadCapasQueCumplen)
-- if f i then 1 + cantidadCapasQueCumplen f pz else cantidadCapasQueCumplen f pz + cantidadCapasQueCumplen f p2

-- Caso f i = True
-- 1 + cantidadCapasQueCumplen f pz + cantidadCapasQueCumplen f p2

-- Caso f i = False
-- cantidadCapasQueCumplen f pz + cantidadCapasQueCumplen f p2

-- Ejercicio 7
-- Definir las siguientes funciones de esquemas sobre listas, utilizando recursión estructural de forma explícita:
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) if f x then x : filter' f xs else filter' f xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f cb [] = cb
foldr' f cb (x:xs) = f x (foldr' f cb xs)

-- recr :: b -> (a -> [a] -> b -> b) -> [a] -> b 

-- foldr1 :: (a -> a -> a) -> [a] -> a 

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

-- Ejercicio 8
-- Demostraciones

-- Ejercicio 9
-- Definir las siguientes funciones utilizando solamente foldr:
-- c = current
-- acc = accumulated

sum :: [Int] -> Int
sum = foldr (\c acc -> c + acc) 0

length :: [a] -> Int
length = foldr (\c acc -> 1 + acc) 0

map :: (a -> b) -> [a] -> [b]
map f = foldr (\c acc -> f c : acc) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\c acc -> if f c then c : acc else acc) []

find :: (a -> Bool) -> [a] -> Maybe a
find f = foldr (\c acc -> if f c then Just c else acc) Nothing

any :: (a -> Bool) -> [a] -> Bool
any f = foldr (\c acc -> f c || acc) False

all :: (a -> Bool) -> [a] -> Bool
all f = foldr (\c acc -> f c && acc) True

countBy :: (a -> Bool) -> [a] -> Int
countBy f = foldr (\c acc -> if f c then 1 + acc else acc) 0

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f = foldr (\c (xs, ys) -> if f c then (c:xs, ys) else (xs, c:ys)) ([], [])

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f = foldr ()

-- scanr :: (a -> b -> b) -> b -> [a] -> [b]

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f = foldr (\c acc -> if f c then c : acc else []) []

take :: Int -> [a] -> [a]
take i xs = foldr f (\n -> []) xs n
    where f x r 0 = []
          f x r n = x : r (n-1)

-- drop :: Int -> [a] -> [a]

-- (!!) :: Int -> [a] -> a

-- Ejercicio 10
-- Indicar cuáles de las siguientes expresiones tienen tipo, y para aquellas que lo tengan, decir cuál es ese tipo:
filter id -- :: [Bool] -> [Bool]
map (\x y z -> (x, y, z)) -- :: [a] -> [(\y z -> (a, y, z))]
map (+) -- :: Num a => [a] -> [a -> a]
filter fst -- [(Bool, a)] -> [(Bool, a)]
filter (flip const (+))  -- :: [Bool] -> [Bool]
map const -- :: [a] -> [b -> a]
map twice -- :: [a -> a] -> [a -> a]
foldr twice -- :: 
zipWith fst -- ::
foldr (\x r z -> (x, z) : r z) (const []) -- ::