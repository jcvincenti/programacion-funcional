-- Seccion 1

data Pizza = Prepizza | Capa Ingrediente Pizza deriving (Show, Eq)
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamón | Queso | Salsa deriving (Show, Eq)

pizza1 = Capa (Aceitunas 2) Prepizza
pizza2 = Capa (Aceitunas 2) (Capa (Aceitunas 4) Prepizza)
-- Ejercicio 2

-- f Prepizza = ...
-- f (Capa i p) = ... f p ...

-- Ejercicio 3

-- cantidadDeCapas, que describe la cantidad de capas de ingredientes de la misma.
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

-- cantidadDeAceitunas, que describe la cantidad de aceitunas que hay en una pizza dada.
cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza = 0
cantidadDeAceitunas (Capa i p) = doCantidadDeAceitunas i + cantidadDeAceitunas p

doCantidadDeAceitunas :: Ingrediente -> Int
doCantidadDeAceitunas (Aceitunas cant) = cant
doCantidadDeAceitunas _ = 0


-- duplicarAceitunas, que dada una pizza, describe otra pizza de forma tal que se cumpla la siguiente propiedad:
--  para todo p. cantidadDeAceitunas (duplicarAceitunas p) = 2 * cantidadDeAceitunas p

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas (Capa i p) = Capa (doDuplicarAceitunas i) (duplicarAceitunas p)
duplicarAceitunas p = p

doDuplicarAceitunas :: Ingrediente -> Ingrediente
doDuplicarAceitunas (Aceitunas cant) = Aceitunas (cant*2)
doDuplicarAceitunas i = i

-- sinLactosa, que describe la pizza resultante de remover todas las capas de queso de una pizza dada.
pizza3 = Capa (Aceitunas 2) (Capa Queso Prepizza)
pizza4 = Capa Queso (Capa Queso Prepizza)
pizza5 = Capa Queso (Capa (Aceitunas 4) Prepizza)

pizza6 = Capa Queso (Capa (Aceitunas 4) (Capa (Aceitunas 4) (Capa Queso (Capa (Aceitunas 4) (Capa Queso Prepizza)))))
pizza7 = Capa Queso (Capa (Aceitunas 4) (Capa (Aceitunas 4) (Capa (Aceitunas 4) (Capa (Aceitunas 4) (Capa Queso Prepizza)))))


sinLactosa :: Pizza -> Pizza
sinLactosa (Capa Queso p) = sinLactosa p
sinLactosa (Capa i p) = Capa i (sinLactosa p)
sinLactosa p = p

-- aptaIntolerantesLactosa, que indica si la pizza dada no tiene queso, osea se cumple la siguiente propiedad:
--      para todo p.  si aptaIntolerantesLactosa p = True entonces p = sinLactosa p 

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa (Capa Queso p) = False
aptaIntolerantesLactosa (Capa i p) = True && aptaIntolerantesLactosa p
aptaIntolerantesLactosa p = True

-- conDescripcionMejorada, que toma una pizza y otra que se construyó con exactamente los mismos ingredientes 
-- pero donde no se agregan aceitunas dos veces seguidas.

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza = Prepizza
conDescripcionMejorada (Capa i p) = juntarAceitunas i (conDescripcionMejorada p)

juntarAceitunas :: Ingrediente -> Pizza -> Pizza
juntarAceitunas (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n+m)) p
juntarAceitunas i p = Capa i p

-- Seccion 2
-- Ejercicio 3
type Nombre = String
data Planilla = Fin | Registro Nombre Planilla deriving (Show, Eq)
data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo deriving (Show, Eq)

planilla1 = Registro "Pepe" (Registro "Juan" Fin)
planilla2 = Registro "Carlos" (Registro "Roberto" (Registro "Francisco" Fin))

equipo1 = Investigador "Carlos"
    (Becario "Roberto")
    (Investigador "Francisco"
        (Becario "Juan")
        (Becario "Gaston")
        (Becario "Fabian")
    )
    (Investigador "Andres"
        (Becario "Lucas")
        (Becario "Gabriel")
        (Investigador "Mauro"
            (Becario "Facundo")
            (Becario "Hernan")
            (Becario "Matias")
        )
    )

-- largoDePlanilla,que describe la cantidad de nombres en una planilla dada.
largoDePlanilla :: Planilla -> Int
largoDePlanilla Fin = 0
largoDePlanilla (Registro n p) = 1 + largoDePlanilla p

-- esta, que toma un nombre y una planilla e indica si en la planilla dada está el nombre dado
esta :: Nombre -> Planilla -> Bool
esta _ Fin = False
esta n (Registro n2 p) = n == n2 || esta n p

-- juntarPlanillas, que toma dos planillas y genera una unica planilla con los registros de ambas planillas
juntarPlanillas :: Planilla -> Planilla -> Planilla
juntarPlanillas Fin r = r
juntarPlanillas (Registro n p1) p2 = Registro n (juntarPlanillas p1 p2)

-- nivelesJerarquicos, que describe la cantidad de niveles jerarquicos de un equipo dado
nivelesJerarquicos :: Equipo -> Int
nivelesJerarquicos (Becario n) = 1
nivelesJerarquicos (Investigador n e1 e2 e3) = 1 + max (nivelesJerarquicos e1) (max (nivelesJerarquicos e2) (nivelesJerarquicos e3))
-- nivelesJerarquicos (Investigador n e1 e2 e3) = 1 + nivelesJerarquicos e1 `max` nivelesJerarquicos e2 `max` nivelesJerarquicos e3

-- cantidadDeIntegrantes, que describe la cantidad de integrantes de un equipo dado
cantidadDeIntegrantes :: Equipo -> Int
cantidadDeIntegrantes (Becario n) = 1
cantidadDeIntegrantes (Investigador n e1 e2 e3) = 1 + cantidadDeIntegrantes e1 + cantidadDeIntegrantes e2 + cantidadDeIntegrantes e3

-- planillaDeIntegrantes, que describe la planilla de integrantes de un equipo dado
planillaDeIntegrantes :: Equipo -> Planilla
planillaDeIntegrantes (Becario n) = Registro n Fin
planillaDeIntegrantes (Investigador n e1 e2 e3) = Registro n (juntarPlanillas (planillaDeIntegrantes e1) (juntarPlanillas (planillaDeIntegrantes e2) (planillaDeIntegrantes e3)))
-- planillaDeIntegrantes (Investigador n e1 e2 e3) = Registro n (planillaDeIntegrantes e1 `juntarPlanillas` planillaDeIntegrantes e2 `juntarPlanillas` planillaDeIntegrantes e3)

-- Seccion 3
-- Ejercicio 3

data Dungeon a = Habitacion a | Pasaje (Maybe a) (Dungeon a) | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a) deriving (Show, Eq)
data Tesoro = Cofre | Oro | Joyas deriving (Show, Eq)

dungeon1 = Bifurcacion Nothing
    (Pasaje Nothing
        (Bifurcacion (Just Cofre)
            (Habitacion Cofre)
            (Habitacion Oro)
        )
    )
    (Bifurcacion Nothing
        (Pasaje (Just Cofre) (Habitacion Joyas))
        (Habitacion Oro)
    )

dungeon2 = Pasaje Nothing
    (Pasaje Nothing
        (Pasaje (Just Cofre)
            (Habitacion Cofre)
        )
    )

dungeon3 = Bifurcacion (Just Cofre)
    (Pasaje (Just Cofre)
        (Bifurcacion (Just Cofre)
            (Habitacion Cofre)
            (Habitacion Cofre)
        )
    )
    (Bifurcacion (Just Cofre)
        (Pasaje (Just Cofre) (Habitacion Cofre))
        (Habitacion Cofre)
    )

-- cantidadDeBifurcaciones, que describe la cantidad de bifurcaciones de un dungeon dado
cantidadDeBifurcaciones :: Dungeon a -> Int
cantidadDeBifurcaciones (Bifurcacion ma d1 d2) = 1 + cantidadDeBifurcaciones d1 + cantidadDeBifurcaciones d2
cantidadDeBifurcaciones (Pasaje ma d1) = cantidadDeBifurcaciones d1
cantidadDeBifurcaciones _ = 0

-- cantidadDePuntosInteresantes, que describe la cantidad de puntos interesantes de un dungeon dado. Los puntos
-- interesantes son los lugares donde puede aparecer un elemento.

-- NOTA: Punto interesante: donde PUEDE aparecer.... Asumo PUEDE = Maybe. Por lo tanto los puntos interesantes son Pasajes y Bifurcaciones
-- De asumir punto interesante como Pasaje, Bifuracaciones y Habitaciones, las soluciones cambian levemente.

cantidadDePuntosInteresantes :: Dungeon a -> Int
cantidadDePuntosInteresantes (Bifurcacion ma d1 d2) = 1 + cantidadDePuntosInteresantes d1 + cantidadDePuntosInteresantes d2
cantidadDePuntosInteresantes (Pasaje ma d1) = 1 + cantidadDePuntosInteresantes d1
cantidadDePuntosInteresantes _ = 0

-- cantidadDePuntosVacios, que describe la cantidad de puntos interesantes del dungeon dado en las que no hay ningun elemento
cantidadDePuntosVacios :: Dungeon a -> Int
cantidadDePuntosVacios (Bifurcacion ma d1 d2) = if estaVacio ma then 0 else 1 + cantidadDePuntosVacios d1 + cantidadDePuntosVacios d2
cantidadDePuntosVacios (Pasaje ma d1) = if estaVacio ma then 0 else 1 + cantidadDePuntosVacios d1
cantidadDePuntosVacios _ = 0

estaVacio :: Maybe a -> Bool
estaVacio (Just a) = True
estaVacio Nothing = False

-- cantidadDePuntosCon, que dado un elemento y un dungeon, describe la cantidad de puntos interesantes del dungeon en las que
-- se encuentra el elemento dado.
cantidadDePuntosCon :: Eq a => a -> Dungeon a -> Int
cantidadDePuntosCon e (Bifurcacion ma d1 d2) = if estaElElemento e ma then 1 else 0 + cantidadDePuntosCon e d1 + cantidadDePuntosCon e d2
cantidadDePuntosCon e (Pasaje ma d1) = if estaElElemento e ma then 1 else 0 + cantidadDePuntosCon e d1
cantidadDePuntosCon e _ = 0

estaElElemento :: Eq a => a -> Maybe a -> Bool
estaElElemento e (Just a) = e == a
estaElElemento e Nothing = False

-- esLineal, que indica si no hay bifurcaciones en un dungeon dado
esLineal :: Dungeon a -> Bool
esLineal (Bifurcacion ma d1 d2) = False
esLineal (Pasaje ma d1) = esLineal d1
esLineal _ = True

-- llenoDe, que dado un elemento y un dungeon, indica si el elemento se encuentra en todas las posiciones del dungeon
llenoDe :: Eq a => a -> Dungeon a -> Bool
llenoDe e (Bifurcacion ma d1 d2) = estaElElemento e ma && llenoDe e d1 && llenoDe e d2
llenoDe e (Pasaje ma d1) = estaElElemento e ma && llenoDe e d1
llenoDe e (Habitacion a) = e == a