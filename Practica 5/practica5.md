# Práctica 5

## Ejercicio 1
Dadas las siguientes definiciones:

```
data Gusto = Chocolate | DulceDeLeche | Frutilla | Sambayon
data Helado = Vasito Gusto | Cucurucho Gusto Gusto | Pote Gusto Gusto Gusto
chocoHelate consH = consH Chocolate
```

determinar el tipo de las siguientes expresiones: 
- ``Vasito``
- ``Chocolate``
- ``Cucurucho``
- ``Sambayon`` 
- ``Pote`` 
- ``chocoHelate ``
- ``chocoHelate Vasito ``
- ``chocoHelate Cucurucho ``
- ``chocoHelate (Cucurucho Sambayon) ``
- ``chocoHelate (chocoHelate Cucurucho)``
- ``chocoHelate (Vasito DulceDeLeche) ``
- ``chocoHelate Pote``
- ``chocoHelate (chocoHelate (Pote Frutilla))``
## Ejercicio 2
Dado el siguiente tipo que pretende representar dígitos binarios:

`data DigBin = O | I`

definir las siguientes funciones:
- ``dbAsInt :: DigBin-> Int``, que dado un símbolo que representa un dígito binario lo transforma en su significado como número.
- ``dbAsBool :: DigBin-> Bool``, que dado un símbolo que representa un dígito binario lo transforma en su significado como booleano.
- ``dbOfBool :: Bool-> DigBin``, que dado un booleano lo transforma en el símbolo que representa a ese booleano.
- ``negDB :: DigBin-> DigBin``, que dado un dígito binario lo transforma en el otro.
## Ejercicio 3
Dado el siguiente tipo que pretende representar dígitos decimales:

`data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9`

definir las siguientes funciones: 
- ``ddAsInt :: DigDec-> Int``, que dado un símbolo que representa un dígito decimal lo transforma en su significado como número.
- ``ddOfInt :: Int-> DigDec``,quedado un número entre 0 y 9 lo transforma en el símbolo que representa a ese dígito. 
- ``nextDD :: DigDec-> DigDec``, que dado un dígito decimal lo transforma en el siguiente según el orden circular dado en la definición. 
- ``prevDD :: DigDec-> DigDec``, que dado un dígito decimal lo transforma en el anterior según el orden circular dado en la definición.

## Ejercicio 4
Dado el siguiente tipo que representa medidas en un software de dibujo como LibreOffice Draw:

`data Medida = Mm Float | Cm Float | Inch Float | Foot Float`

y la siguiente tabla de conversión:

escribir las siguientes funciones:
- ``asMm :: Medida-> Medida``, que dada una medida cualquiera la transforma en una medida en milímetros que aproxima la dada según la conversión establecida. 
- ``asCm :: Medida-> Medida``, que dada una medida cualquiera la transforma en una medida en centímetros que aproxima la dada según la conversión establecida. 
- ``asInch :: Medida-> Medida``, que dada una medida cualquiera la transforma en una medida en pulgadas que aproxima la dada según la conversión establecida. 
- ``asFoot :: Medida-> Medida``, que dada una medida cualquiera la transforma en una medida en pies que aproxima la dada según la conversión establecida.

## Ejercicio 5
Dadas las siguientes definiciones:

```
data Shape = Circle Float | Rect Float Float
construyeShNormal :: (Float -> Shape) -> Shape
construyeShNormal c = c 1.0
```

Determinar el tipo de las siguientes expresiones: 
- ``uncurry Rect ``
- ``construyeShNormal (flip Rect 5.0) ``
- ``compose (uncurry Rect) swap ``
- ``uncurry Cucurucho ``
- ``uncurry Rect swap ``
- ``compose uncurry Pote ``
- ``compose Just ``
- ``compose uncurry (Pote Chocolate)``
## Ejercicio 6
Para cada una de las expresiones del ejercicio anterior que denoten funciones, construir una expresión aplicándola.

## Ejercicio 7
Dado el tipo Set definido en la clase teórica

`data Set a = S (a -> Bool)`

definir las siguientes funciones sobre él:
- ``belongs :: Set a-> a-> Bool``, que dado un conjunto, describe la función que indica si un elemento dado pertenece a ese conjunto. 
- ``empty :: Set a``, que describe el conjunto vacío. 
- ``singleton :: a-> Set a``, que dado un elemento describe un conjunto que contiene a ese único elemento. 
- ``union :: Set a-> Set a-> Set a``, que dados dos conjuntos, describe al conjunto que resulta de la unión de ambos. 
- ``intersection :: Set a-> Set a-> Set a``, que dados dos conjuntos, describe al conjunto que resulta de la intersección de ambos.

## Ejercicio 8
Dados los siguientes tipos que modelan los posibles resultados de computar con excepciones:

```
data MayFail a = Raise Exception | Ok a
data Exception = DivByZero | NotFound | NullPointer | Other String
type ExHandler a = Exception -> a
```

definir la función ``tryCatch :: MayFail a -> (a -> b) -> ExHandler b -> b`` que dada una computación que puede fallar, una función que indica cómo continuar si no falla, y un manejador de los casos de falla, expresa la computación completa.
Un ejemplo que utiliza esta función sería el siguiente:

```
sueldoGUIE :: Nombre -> [Empleado] -> GUI Int
sueldoGUIE nombre empleados =
    tryCatch (lookupE nombre empleados)
        mostrarInt
        (\e -> case e of
            NotFound -> ventanaError msgNotEmployee
            _        -> error msgUnexpected)
        where   msgNotEmployee = "No es empleado de la empresa"
                msgUnexpected  = "Error inesperado"
```
sabiendo que:

```
mostrarInt :: Int -> GUI Int
ventanaError :: String -> GUI a
lookupE :: Nombre -> [Empleado] -> MayFail Int
```
