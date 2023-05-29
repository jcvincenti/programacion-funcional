data NExp = Var Variable | NCte Int | NBOp NBinOp NExp NExp
data NBinOp = Add | Sub | Mul | Div | Mod | Pow
type Variable = String

f (Var var) = ...
f (NCte n) = ...
f (NBOp op e1 e2) = ... f e1 ... f e2 ...

-- El TAD Memoria cuya interfaz es la siguiente:
-- enBlanco :: Memoria, que describe una memoria vacía.
-- cuantoVale :: Variable -> Memoria -> Maybe Int, que describe
--      el número asociado a la variable dada en la memoria dada.
-- recordar :: Variable -> Int -> Memoria -> Memoria, que la
--      memoria resultante de asociar el número dado a la variable dada en la memoria dada.
-- variables :: Memoria -> [Variable], que describe las variables que la memoria recuerda.

-- evalNExp :: NExp -> Memoria -> Int
-- Describe el número resultante de evaluar la expresión dada a partir de la memoria dada
fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = "La variable no esta definida"

evalNExp :: NExp -> Memoria -> Int
evalNExp (Var var) = fromJust(cuantoVale var)
evalNExp (NCte n) = n
evalNExp (NBOp op e1 e2) = evalOp op (evalNExp e1) (evalNExp e2)

evalOp :: NBinOp -> Int -> Int -> Int
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mul = (*)
evalOp Div = div
evalOp Mod = mod
evalOp Pow = (^)

-- cfNExp :: NExp -> NExp
-- Describe una expresión con el mismo significado que la dada, pero simplificada y reemplazando las
-- subexpresiones que no dependan de la memoria por su expresión más sencilla.
-- La resolución debe ser exclusivamente simbólica.

cfNExp :: NExp -> NExp
cfNExp (Var var) = Var var
cfNExp (NCte n) = NCte n
cfNExp (NBOp op e1 e2) = simplOp op (cfNExp e1) (cfNExp e2)

simplOp :: NBinOp -> NExp -> NExp -> NExp
simplOp Add (NCte 0) e2 = e2
simplOp Add e1 (NCte 0) = e1
simplOp Sub e1 (NCte 0) = e1
simplOp Mul (NCte 0) e2 = NCte 0
simplOp Mul e1 (NCte 0) = NCte 0
simplOp Mul (NCte 1) e2 = e2
simplOp Mul e1 (NCte 1) = e1
simplOp Div e1 (NCte 1) = e1
simplOp Pow e1 (NCte 1) = e1
simplOp Pow e1 (NCte 0) = NCte 1
simplOp op e1 e2 = NBOp op e1 e2

data BExp = BCte Bool | Not BExp | And BExp BExp | Or BExp BExp | ROp RelOp NExp NExp
data RelOp = Eq | NEq -- Equal y NotEqual
    | Gt | GEq -- Greater y GreaterOrEqual
    | Lt | LEq -- Lower y LowerOrEqual

f (BCte b) = ...
f (Not e) = ...
f (And e1 e2) = ... f e1 ... f e2 ...
f (Or e1 e2) = ... f e1 ... f e2 ...
f (ROp op e1 e2) = ... f e1 ... f e2 ...

-- evalBExp :: BExp -> Memoria -> Bool
-- Describe el booleano que resulta de evaluar la expresión dada a partir de la memoria dada.

evalBExp :: BExp -> Memoria -> Bool
evalBExp (BCte b) m = b
evalBExp (Not e) m = evalBExp e m
evalBExp (And e1 e2) m = evalBExp e1 m && evalBExp e2 m
evalBExp (Or e1 e2) m = evalBExp e1 m || evalBExp e2 m
evalBExp (ROp op e1 e2) m = evalROp op (evalNExp e1 m) (evalNExp e2 m)

evalROp :: RelOp -> Int -> Int -> Bool
evalROp Eq = (==)
evalROp NEq = (/=)
evalROp Gt = (>)
evalROp GEq = (>=)
evalROp Lt = (<)
evalROp LEq = (<=)

-- cfBExp :: BExp -> BExp
-- Describe una expresión con el mismo significado que la dada, pero reemplazando las
-- subexpresiones que no dependan de la memoria por su expresión más sencilla.
-- La resolución debe ser exclusivamente simbólica.

cfBExp :: BExp -> BExp
cfBExp (BCte b) = BCte b
cfBExp (Not e) = Not (cfBExp e)
cfBExp (And e1 e2) = simplAnd (cfBExp e1) (cfBExp e2)
cfBExp (Or e1 e2) = simplOr (cfBExp e1) (cfBExp e2)
cfBExp (ROp op e1 e2) = ROp op (cfNExp e1) (cfNExp e2)

simplOr :: BExp -> BExp -> BExp
simplOr (BCte True) e2 = BCte True
simplOr e1 (BCte True) = BCte True
simplOr (BCte False) e2 = e2
simplOr e1 (BCte False) = e1
simplOr e1 e2 = Or e1 e2

simplAnd :: BExp -> BExp -> BExp
simplAnd (BCte True) e2 = e2
simplAnd e1 (BCte True) = e1
simplAnd (BCte False) e2 = BCte False
simplAnd e1 (BCte False) = BCte False
simplAnd e1 e2 = And e1 e2


data Programa = Prog Bloque
type Bloque = [Comando]
type Nombre = String
data Comando = Assign Nombre NExp | If BExp Bloque Bloque | While BExp Bloque

-- evalProg :: Programa -> Memoria -> Memoria
-- Describe la memoria resultante de evaluar el programa dado a partir de la memoria dada.
evalProg :: Programa -> Memoria -> Memoria
evalProg (Prog blq) m = evalBlq blq m

-- evalBlq :: Bloque -> Memoria -> Memoria
-- Describe la memoria resultante de evaluar el bloque dado a partir de la memoria dada.
evalBlq :: Bloque -> Memoria -> Memoria
evalBlq [] m = id
evalBlq (c:cs) m = evalBlq cs (evalCom c m)

-- evalCom :: Comando -> Memoria -> Memoria
-- Describe la memoria resultante de evaluar el comando dado a partir de la memoria dada.

evalCom (Assign var e) m = recordar var (evalNExp e) m
evalCom (If b blq1 blq2) m = if evalBExp b m then evalBlq blq1 m else evalBlq blq2 m
evalCom (While b blq) m = if evalBExp b m then evalCom (While b blq) (evalBlq blq m) else m
