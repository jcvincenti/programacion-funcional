-- Seccion 1

-- Ejercicio 1

-- f [] = ...
-- f (x:xs) = ... f xs

ej1 = [1, 2, 3, 4]
ej2 = [1, 2, 3, 4, 5]

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

elem' :: Eq a => a -> [a] -> Bool
elem' e [] = False
elem' e (x:xs) = x == e || elem' e xs

all' :: (a -> Bool) -> [a] -> Bool
all' f [] = True
all' f (x:xs) = f x && all' f xs

any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs) = f x || any' f xs

count' :: (a -> Bool) -> [a] -> Int
count' f [] = 0
count' f (x:xs) = if f x then 1 else 0 + count' f xs

subset' :: Eq a => [a] -> [a] -> Bool
subset' [] _ = True
subset' (x:xs) ys = elem' x ys && subset' xs ys

subset2 :: Eq a => [a] -> [a] -> Bool
subset2 xs ys = all' (flip elem' ys) xs

-- (++) :: [a] -> [a] -> [a]
-- (++) [] xs = xs
-- (++) (x:xs) ys = x : (++) xs ys

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' [] [] = []
zip' (x:xs) [] = []
zip' [] (y:ys) = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' (x:xs) = agregarPar x (unzip' xs)

agregarPar :: (a, b) -> ([a], [b]) -> ([a], [b])
agregarPar (a, b) (xs, ys) = (a:xs, b:ys)

-- Ejercicio 2

-- Prop: ¿para todo xs e ys. length (xs ++ ys) = length xs + length ys?
-- Dem: Sean zs y ws listas cualesquiera. Por ppio de ind. estr. en zs

-- Caso base: zs = []
-- ¿length ([] ++ ws) = length [] + length ws?

-- Caso inductivo: zs = x:xs'
-- HI: length (xs' ++ ws) = length xs' + length ws
-- TI: ¿length ((x:xs') ++ ws) = length (x:xs') + length ws?

-- Dem: Caso base. zs = [] ¿length ([] ++ ws) = length [] + length ws?

--   length ([] ++ ws)               |            length [] + length ws
-- =                     (++)        | =                                  (length)
--   length ws                       |            0 + length ws
--                                   | =                                  (aritm.)
--                                   |            length ws

-- Dem: Caso inductivo. zs = x:xs' ¿length ((x:xs') ++ ws) = length (x:xs') + length ws?

--   length (x:xs' ++ ws)            |            length (x:xs') + length ws
-- =                     (++)        | =                                  (length)
--   length (x : xs' ++ ws)          |            1 + length xs' + length ws
-- =                                 |
--   1 + length (xs' ++ ws)          |
-- =                     (HI)        |
--   1 + length xs' + length ws      |


-- Prop: ¿count (const True) = length?
-- Dem: Por ppio de extensionalidad. para todo zs count (const True) zs = length zs
--      Sea zs una lista. Por ppio de ind. estr. en zs

-- Caso base: zs = []
-- ¿count (const True) [] = length []?

-- Caso inductivo: zs = x:xs
-- HI: count (const True) xs = length xs
-- TI: ¿count (const True) (x:xs) = length (x:xs)?

-- Dem: Caso base: zs = []. ¿count (const True) [] = length []?
--  count (const True) []           |            length []
-- =                     (count)    | =                                  (length)
--  0                               |            0

-- Dem: Caso inductivo: zs = x:xs. ¿count (const True) (x:xs) = length (x:xs)?
--  count (const True) (x:xs)                                   |            length (x:xs)
-- =                                                 (count)    | =                                  (length)
--  if const True x then 1 else 0 + count (const True) xs       |            1 + length (xs)
-- =                                                 (const)    |
--  if True then 1 else 0 + count (const True) xs               |
-- =                                          (if-then-else)    |
--  1 + count (const True) xs                                   |
-- =                                                    (HI)    |
--  1 + length xs                                               |


-- Prop: ¿elem = any . (==)?
-- Dem: Por ppio de extensionalidad. para todo e. para todo zs. elem e zs = (any . (==)) e zs
--      Sea zs una lista y e un elemento. Por ppio de ind. estr. en zs
--      Por def de .
--      elem e zs = any ((==) e) zs

-- Caso base: zs = []
-- ¿elem e [] = any ((==) e) []?

-- Caso inductivo: zs = x:xs
-- HI: elem e xs = any ((==) e) xs
-- TI: ¿elem e (x:xs) = any ((==) e) (x:xs)?

-- Dem: Caso base: zs = []. ¿elem e [] = any ((==) e) []?
--  elem e []                       |   any ((==) e) []
-- =                     (elem)     | =                     (any)
--  False                           |   False

-- Dem: Caso inductivo: zs = x:xs. ¿elem e (x:xs) = any ((==) e) (x:xs)?

--  elem e (x:xs)                       |   any ((==) e) (x:xs)
-- =                     (elem)         | =                     (any)
--  x == e || elem e xs                 |   e == x || any ((==) e) xs
-- =                     (HI)           |
--  x == e || any ((==) e) xs           |




-- Seccion 2

-- Ejercicio 1 a

-- f [] = ...
-- f (x:xs) = ... f xs

many 0 f x = x
many n f x = f (many (n-1) f x)

data N = Z | S N deriving (Show, Eq)

ejN1 = S (S Z)
ejN2 = S (S (S (S (S (S Z)))))

evalN :: N -> Int
evalN Z = 0
evalN (S n) = 1 + evalN n

addN :: N -> N -> N
addN Z n2 = n2
addN (S n) n2 = S (addN n n2)

prodN :: N -> N -> N
prodN Z n2 = Z
prodN (S n) n2 = addN n2 (prodN n n2)

int2N :: Int -> N
int2N 0 = Z
int2N i = S (int2N (i-1))

-- Ejercicio 1 b

-- Prop: para todo n1. para todo n2 ¿evalN (addN n1 n2) = evalN n1 + evalN n2?
-- Dem: Sean n1 y n2 Por ppio de ind. sobre n1

-- Caso base: n1 = Z
-- ¿evalN (addN Z n2) = evalN Z + evalN n2?

-- Caso inductivo: n1 = S n
-- HI: evalN (addN n n2) = evalN n + evalN n2
-- TI: ¿evalN (addN (S Z) n2) = evalN (S Z) + evalN n2?

-- Desarrollo

-- Caso base
--  evalN (addN Z n2)                   |   evalN Z + evalN n2
-- =                     (addN)         | =                     (evalN)
--  evalN n2                            |   0 + evalN n2
--                                      | =                     (aritm)
--                                      |   evalN n2

-- Caso inductivo
--  evalN (addN (S n) n2)               |   evalN (S n) + evalN n2
-- =                     (addN)         | =                     (evalN)
--  evalN (S (addN n n2))               |   1 + evalN n + evalN n2
-- =                     (evalN)        |
--  1 + evalN (addN n n2)               |
-- =                     (HI)           |
--  1 + evalN n + evalN n2              |


-- Prop: para todo n1. para todo n2 ¿evalN (prodN n1 n2) = evalN n1 * evalN n2?
-- Dem: Sean n1 y n2 Por ppio de ind. sobre n1

-- Caso base: n1 = Z
-- ¿evalN (prodN Z n2) = evalN Z * evalN n2?

-- Caso inductivo: n1 = S n
-- HI: evalN (prodN n n2) = evalN n * evalN n2
-- TI: ¿evalN (prodN (S Z) n2) = evalN (S Z) * evalN n2?

-- Desarrollo

-- Caso base
--  evalN (prodN Z n2)                   |   evalN Z * evalN n2
-- =                     (prodN)         | =                     (evalN)
--  evalN Z                              |   0 * evalN n2
-- =                     (evalN)         | =                     (aritm)
--  0                                    |   0

-- Caso inductivo
--  evalN (prodN (S n) n2)               |   evalN (S n) * evalN n2
-- =                     (prodN)         | =                     (evalN)
--  evalN (addN n2 (prodN n n2))         |   (1 + evalN n) * evalN n2
--                                       | =                     (aritm)
--                                       |   evalN n2 + evalN n * evalN n2
--                                       | =                     (HI)
--                                       |   evalN n2 + evalN (prodN n n2)
--                                       | =                     (por dem. ejercicio anterior)
--                                       |   evalN (addN n2 (prodN n n2))


-- Prop: ¿int2N . evalN = id?
-- Dem: Por ppio de extensionalidad. para todo n1. int2N . evalN n1 = id n1
--      Por definicion de .
--      ¿int2N (evalN n1) = id n1?
--      Sea n1 elemento del conjunto N. Por ppio de ind. estr. en n1

-- Caso base: n1 = Z
-- ¿int2N (evalN Z) = id Z?

-- Caso inductivo: n1 = S n
-- HI: int2N (evalN n) = id n
-- TI: ¿int2N (evalN (S n)) = id (S n)?

-- Desarrollo
-- Caso base
--  int2N (evalN Z)                      |   id Z
-- =                     (evalN)         | =                     (id)
--  int2N 0                              |   Z
-- =                     (int2N)         |
--  Z                                    |

-- Caso inductivo
--  int2N (evalN (S n))                  |   id (S n)
-- =                     (evalN)         | =                     (id)
--  int2N (1 + evalN n)                  |   S n


-- Prop: evalN . int2N = id?
-- Dem: Por ppio de extensionalidad. para todo n. evalN . int2N n = id n
--      Por definicion de .
--      ¿evalN (int2N n) = id n?
--      Sea n un numero cualquiera. Por ppio de ind. estr. en n

-- Caso base: n = 0
-- ¿evalN (int2N 0) = id 0?

-- Caso inductivo: n = x
-- HI: evalN (int2N x) = id x
-- TI: ¿evalN (int2N x) = id x?

-- Desarrollo

-- Caso base
--  evalN (int2N 0)                      |   id 0
-- =                     (int2N)         | =                     (id)
--  evalN Z                              |   0
-- =                     (evalN)         |
--  0                                    |

-- Caso inductivo
--  evalN (int2N x)                      |   id x
-- =                     (int2N)         | =                     (id)
--  evalN (S (int2N (x-1)))              |   x


-- Ejercicio 3
data DigBin = O | I
type NBin = [DigBin]

dbAsInt :: DigBin -> Int
dbAsInt O = 0
dbAsInt I = 1

evalNB :: NBin -> Int
evalNB [] = 0
evalNB (x:xs) = dbAsInt x + 2 * evalNB xs

normalizarNB :: NBin -> NBin
normalizarNB [] = []
normalizarNB (x:xs) = norm x (normalizarNB xs)

norm :: DigBin -> [DigBin] -> [DigBin]
norm O [] = []
norm x xs = x:xs

succNB :: NBin -> NBin
succNB [] = [I]
succNB (O:xs) = I : xs
succNB (I:xs) = O : succNB xs

nb2n :: NBin -> N
nb2n [] = Z
nb2n xs = int2N (evalNB xs)

-- n2nb :: N -> NBin
-- n2nb Z = []
-- n2nb (S n) = addNB [I] (n2nb n)

-- Seccion 3
-- Ejercicio 1

data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA deriving (Show, Eq)

evalExpA :: ExpA -> Int
evalExpA (Cte n) = n
evalExpA (Suma n1 n2) = evalExpA n1 + evalExpA n2
evalExpA (Prod n1 n2) = evalExpA n1 * evalExpA n2

esCero :: ExpA -> Bool
esCero (Cte 0) = True
esCero e = False

esUno :: ExpA -> Bool
esUno (Cte 1) = True
esUno e = False

ej = Suma (Cte 0) (Suma (Cte 1) (Prod (Cte 0) (Cte 1)))

simplificarExpA :: ExpA -> ExpA
simplificarExpA (Suma n1 n2)
  | esCero n1 = simplificarExpA n2
  | esCero n2 = simplificarExpA n1
  | otherwise = Suma (simplificarExpA n1) (simplificarExpA n2)
simplificarExpA (Prod n1 n2)
  | esUno n1 = simplificarExpA n2
  | esUno n2 = simplificarExpA n1
  | otherwise = Prod (simplificarExpA n1) (simplificarExpA n2)
simplificarExpA (Cte n) = Cte n

cantidadDeSumaCero :: ExpA -> Int
cantidadDeSumaCero (Cte n) = 0
cantidadDeSumaCero (Suma n1 n2) = hayCero n1 n2 + cantidadDeSumaCero n1 + cantidadDeSumaCero n2
cantidadDeSumaCero (Prod n1 n2) = cantidadDeSumaCero n1 + cantidadDeSumaCero n2

hayCero (Cte 0) n2 = 1
hayCero n1 (Cte 0) = 1
hayCero n1 n2 = 0