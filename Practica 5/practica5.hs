import Data.List (elemIndex)
import Data.Maybe
import System.Win32.DebugApi (DebugEventInfo(Exception))
-- Funciones auxiliares

fst = \(a, b) -> a
snd = \(a, b) -> b
suma = \x -> \y -> x + y

doble = \x -> x + x
twice = \f -> \x -> f (f x)
compose = \f -> \g -> \x -> f(g x)
swap = \(x, y) -> (y, x)
--------------------------

-- Ejercicio 1
data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon  deriving (Show, Eq)
data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto  deriving (Show, Eq)
chocoHelate consH = consH Chocolate

-- a) Vasito :: Gusto -> Helado
-- b) Chocolate :: Gusto
-- c) Cucurucho :: Gusto -> Gusto -> Helado
-- d) Sambayon :: Gusto
-- e) Pote :: Gusto -> Gusto -> Gusto -> Helado
-- f) chocoHelate :: (Gusto -> a) -> a
-- g) chocoHelate Vasito :: Helado
-- h) chocoHelate Cucurucho :: Gusto -> Helado
-- i) chocoHelate (Cucurucho Sambayon) :: Helado
-- j) chocoHelate (chocoHelate Cucurucho) :: Helado
-- k) chocoHelate (Vasito DulceDeLeche) :: No tiene tipo
-- l) chocoHelate Pote :: Gusto -> Gusto -> Helado
-- m) chocoHelate (chocoHelate (Pote Frutilla)) :: Helado

-- Ejercicio 2
data DigBin = O | I

dbAsInt :: DigBin -> Int
dbAsInt O = 0
dbAsInt I = 1

dbAsBool :: DigBin -> Bool
dbAsBool O = False
dbAsBool I = True

dbOfBool :: Bool-> DigBin
dbOfBool False = O
dbOfBool True = I

negDB :: DigBin-> DigBin
negDB I = O
negDB O = I

-- Ejercicio 3
data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Show, Eq)

ddAsInt :: DigDec -> Int -- que dado un símbolo que representa un dígito decimal lo transforma en su significado como número.
ddAsInt i = fromJust(i `elemIndex` [D0, D1, D2, D3, D4, D5, D6, D7, D8, D9])

ddOfInt :: Int -> DigDec -- que dado un número entre 0 y 9 lo transforma en el símbolo que representa a ese dígito.
ddOfInt i = [D0, D1, D2, D3, D4, D5, D6, D7, D8, D9]!!i
nextDD :: DigDec -> DigDec -- que dado un dígito decimal lo transforma en el siguiente según el orden circular dado en la definición.
nextDD dd = let i = ddAsInt dd in ddOfInt (if i > 8 then 0 else i+1)

prevDD :: DigDec-> DigDec -- que dado un dígito decimal lo transforma en el anterior según el orden circular dado en la definición.
prevDD dd = let i = ddAsInt dd in ddOfInt (if i == 0 then 9 else i-1)

-- Ejercicio 4
data Medida = Mm Float | Cm Float | Inch Float | Foot Float deriving (Show, Eq)

asMm :: Medida -> Medida -- que dada una medida cualquiera la transforma en una medida en milímetros que aproxima la dada según la conversión establecida.
asMm (Mm a) = Mm a
asMm (Cm a) = Mm (a * 10)
asMm (Inch a) = Mm (a * 25.4)
asMm (Foot a) = Mm (a * 304.8)

asCm :: Medida -> Medida -- que dada una medida cualquiera la transforma en una medida en centímetros que aproxima la dada según la conversión establecida. 
asCm (Mm a) = Cm (a * 0.1)
asCm (Cm a) = Cm a
asCm (Inch a) = Cm (a * 2.54)
asCm (Foot a) = Cm (a * 30.48)

asInch :: Medida -> Medida -- que dada una medida cualquiera la transforma en una medida en pulgadas que aproxima la dada según la conversión establecida.
asInch (Mm a) = Inch (a * 0.039)
asInch (Cm a) = Inch (a * 0.394)
asInch (Inch a) = Inch a
asInch (Foot a) = Inch (a * 12)
asFoot :: Medida -> Medida -- que dada una medida cualquiera la transforma en una medida en pies que aproxima la dada según la conversión establecida.
asFoot (Mm a) = Foot (a * 0.003)
asFoot (Cm a) = Foot (a * 0.033)
asFoot (Inch a) = Foot (a * 0.083)
asFoot (Foot a) = Foot a

-- Ejercicio 5
data Shape = Circle Float | Rect Float Float deriving (Show, Eq)

construyeShNormal :: (Float -> Shape) -> Shape
construyeShNormal c = c 1.0

-- uncurry Rect :: (Float, Float) -> Shape
-- construyeShNormal (flip Rect 5.0) :: Shape
-- compose (uncurry Rect) swap :: (Float, Float) -> Shape
-- uncurry Cucurucho :: (Gusto, Gusto) -> Helado
-- uncurry Rect swap :: No tiene tipo
-- compose uncurry Pote :: Gusto -> (Gusto, Gusto) -> Helado
-- compose Just :: (a -> b) -> a -> Maybe b
-- compose uncurry (Pote Chocolate) :: No tiene tipo

-- Ejercicio 6
-- uncurry Rect :: (Float, Float) -> Shape
-- uncurry Rect (1.0, 2.0)

-- compose (uncurry Rect) swap :: (Float, Float) -> Shape
-- compose (uncurry Rect) swap (1.0, 2.0)

-- uncurry Cucurucho :: (Gusto, Gusto) -> Helado
-- uncurry Cucurucho (Chocolate, Frutilla)

-- compose uncurry Pote :: Gusto -> (Gusto, Gusto) -> Helado
-- compose uncurry Pote Chocolate (Frutilla, Sambayon)

-- compose Just :: (a -> b) -> a -> Maybe b
-- compose Just Vasito Chocolate

-- Ejercicio 7
data Set a = S (a -> Bool)

belongs :: Set a -> a -> Bool -- que dado un conjunto, describe la función que indica si un elemento dado pertenece a ese conjunto.
belongs (S f) a = f a

-- ???????
empty :: Set a -- que describe el conjunto vacío.
empty = S (const False)

-- ???????
singleton :: Eq a => a -> Set a -- que dado un elemento describe un conjunto que contiene a ese único elemento.
singleton a = S (== a)

union :: Set a -> Set a -> Set a -- que dados dos conjuntos, describe al conjunto que resulta de la unión de ambos.
union (S f1) (S f2) = S (\n -> f1 n || f2 n)

intersection :: Set a-> Set a-> Set a -- que dados dos conjuntos, describe al conjunto que resulta de la intersección de ambos.
intersection (S f1) (S f2) = S (\n -> f1 n && f2 n)

-- Ejercicio 8
data MayFail a = Raise Exception | Ok a
data Exception = DivByZero | NotFound | NullPointer | Other String
type ExHandler a = Exception -> a

tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b
tryCatch mf f eh = case mf of Ok a -> f a
                              Raise e -> eh e

