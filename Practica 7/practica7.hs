--------------------------------------------- Seccion 1 ---------------------------------------------
data Pizza = Prepizza | Capa Ingrediente Pizza deriving (Show, Eq)
data Ingrediente = Aceitunas Int | Anchoas | Cebolla | Jamón | Queso | Salsa | MezclaRara deriving (Show, Eq)

-- Ejercicio 1

-- Pizza es un conjunto inductivo con las siguientes reglas:

-- Caso base:
-- - Prepizza está en el conjunto Pizza
-- Caso inductivo:
-- - Sea i un Ingrediente y p una Pizza, Capa i p está en el conjunto Pizza

-- Ingrediente es un conjunto donde sus elementos son:
-- Sea n un Int, Aceituas n es un Ingrediente
-- Jamon es un Ingrediente
-- Queso es un Ingrediente
-- Salsa es un Ingrediente

pizza1 = Capa (Aceitunas 2) Prepizza
pizza2 = Capa (Aceitunas 2) (Capa (Aceitunas 4) Prepizza)
-- Ejercicio 2

-- f Prepizza = ...
-- f (Capa i p) = ... f p

-- Ejercicio 3

-- cantidadDeCapas, que describe la cantidad de capas de ingredientes de la misma.
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

-- cantidadDeAceitunas, que describe la cantidad de aceitunas que hay en una pizza dada.
cantidadDeAceitunas :: Pizza -> Int
cantidadDeAceitunas Prepizza = 0
cantidadDeAceitunas (Capa i p) = aceitunas i + cantidadDeAceitunas p

aceitunas :: Ingrediente -> Int
aceitunas (Aceitunas cant) = cant
aceitunas _ = 0

-- duplicarAceitunas, que dada una pizza, describe otra pizza de forma tal que se cumpla la siguiente propiedad:
--  para todo p. cantidadDeAceitunas (duplicarAceitunas p) = 2 * cantidadDeAceitunas p

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas (Capa i p) = Capa (duplicarSiAceitunas i) (duplicarAceitunas p)
duplicarAceitunas Prepizza = Prepizza

duplicarSiAceitunas :: Ingrediente -> Ingrediente
duplicarSiAceitunas (Aceitunas cant) = Aceitunas (cant*2)
duplicarSiAceitunas i = i

-- sinLactosa, que describe la pizza resultante de remover todas las capas de queso de una pizza dada.
pizza3 = Capa (Aceitunas 2) (Capa Queso Prepizza)
pizza4 = Capa Queso (Capa Queso Prepizza)
pizza5 = Capa Queso (Capa (Aceitunas 4) Prepizza)

pizza6 = Capa Queso (Capa (Aceitunas 4) (Capa (Aceitunas 4) (Capa Queso (Capa (Aceitunas 4) (Capa Queso Prepizza)))))
pizza7 = Capa Queso (Capa (Aceitunas 4) (Capa (Aceitunas 4) (Capa (Aceitunas 4) (Capa (Aceitunas 4) (Capa Queso Prepizza)))))


sinLactosa :: Pizza -> Pizza
sinLactosa Prepizza = Prepizza
sinLactosa (Capa Queso p) = sinLactosa p
sinLactosa (Capa i p) = Capa i (sinLactosa p)

-- aptaIntolerantesLactosa, que indica si la pizza dada no tiene queso, osea se cumple la siguiente propiedad:
--      para todo p.  si aptaIntolerantesLactosa p = True entonces p = sinLactosa p 

aptaIntolerantesLactosa :: Pizza -> Bool
aptaIntolerantesLactosa Prepizza = True
aptaIntolerantesLactosa (Capa Queso p) = False
aptaIntolerantesLactosa (Capa i p) = aptaIntolerantesLactosa p

-- conDescripcionMejorada, que toma una pizza y otra que se construyó con exactamente los mismos ingredientes 
-- pero donde no se agregan aceitunas dos veces seguidas.

conDescripcionMejorada :: Pizza -> Pizza
conDescripcionMejorada Prepizza = Prepizza
conDescripcionMejorada (Capa i p) = juntarAceitunas i (conDescripcionMejorada p)

juntarAceitunas :: Ingrediente -> Pizza -> Pizza
juntarAceitunas (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n+m)) p
juntarAceitunas i p = Capa i p

juntarPizzas :: Pizza -> Pizza -> Pizza
juntarPizzas Prepizza pz = pz
juntarPizzas (Capa i p) pz = Capa i (juntarPizzas p pz)

combinarIngredientes :: Ingrediente -> Ingrediente -> Ingrediente
combinarIngredientes (Aceitunas n) (Aceitunas m) = Aceitunas (n+m)
combinarIngredientes i i2 = MezclaRara

combinarIngredientesPorCapa :: Pizza -> Pizza -> Pizza
combinarIngredientesPorCapa Prepizza pz = pz
combinarIngredientesPorCapa pz Prepizza = pz
combinarIngredientesPorCapa (Capa i1 pz1) (Capa i2 pz2) = Capa (combinarIngredientes i1 i2) (combinarIngredientesPorCapa pz1 pz2)

-- Ejercicio 4
-- Demostrar:

-- a) cantidadDeAceitunas Prepizza = cantidadDeAceitunas (conDescripcionMejorada Prepizza)
-- lado izq
-- cantidadDeAceitunas Prepizza
-- 0

-- lado der
-- cantidadDeAceitunas (conDescripcionMejorada Prepizza)
-- cantidadDeAceitunas Prepizza
-- 0

-- b) cantidadDeAceitunas (Capa Queso Prepizza) = cantidadDeAceitunas (conDescripcionMejorada (Capa Queso Prepizza))
-- lado izq
-- cantidadDeAceitunas (Capa Queso Prepizza)
-- doCantidadDeAceitunas Queso + cantidadDeAceitunas Prepizza
-- 0 + cantidadDeAceitunas Prepizza
-- 0 + 0
-- 0

-- lado der
-- cantidadDeAceitunas (conDescripcionMejorada (Capa Queso Prepizza))
-- cantidadDeAceitunas (juntarAceitunas Queso (conDescripcionMejorada Prepizza))
-- cantidadDeAceitunas (juntarAceitunas Queso Prepizza)
-- cantidadDeAceitunas (Capa Queso Prepizza)
-- doCantidadDeAceitunas Queso + cantidadDeAceitunas Prepizza
-- 0 + cantidadDeAceitunas Prepizza
-- 0 + 0
-- 0

-- c) cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))
--      = cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza)))

-- lado izq
-- cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))
-- doCantidadDeAceitunas (Aceitunas 8) + cantidadDeAceitunas (Capa Queso Prepizza)
-- 8 + cantidadDeAceitunas (Capa Queso Prepizza)
-- 8 + doCantidadDeAceitunas Queso + cantidadDeAceitunas Prepizza
-- 8 + 0 + cantidadDeAceitunas Prepizza
-- 8 + 0 + 0
-- 8

-- lado der
-- cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza)))
-- cantidadDeAceitunas (juntarAceitunas (Aceitunas 8) (conDescripcionMejorada (Capa Queso Prepizza)))
-- cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))
-- doCantidadDeAceitunas (Aceitunas 8) + cantidadDeAceitunas (Capa Queso Prepizza)
-- 8 + doCantidadDeAceitunas Queso + cantidadDeAceitunas Prepizza
-- 8 + 0 + cantidadDeAceitunas Prepizza
-- 8 + 0 + 0
-- 8

-- d) cantidadDeAceitunas (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza)))
--      = cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza))))

-- lado izq
-- cantidadDeAceitunas (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza)))
-- doCantidadDeAceitunas (Aceitunas 9) + cantidadDeAceitunas (Capa (Aceitunas 8) (Capa Queso Prepizza))
-- 9 + doCantidadDeAceitunas (Aceitunas 8) + cantidadDeAceitunas (Capa Queso Prepizza)
-- 9 + 8 + doCantidadDeAceitunas Queso + cantidadDeAceitunas Prepizza
-- 9 + 8 + 0 + cantidadDeAceitunas Prepizza
-- 9 + 8 + 0 + 0
-- 17

-- lado der
-- cantidadDeAceitunas (conDescripcionMejorada (Capa (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza))))
-- cantidadDeAceitunas (juntarAceitunas (Aceitunas 9) (conDescripcionMejorada (Capa (Aceitunas 8) (Capa Queso Prepizza))))
-- cantidadDeAceitunas (juntarAceitunas (Aceitunas 9) (juntarAceitunas (Aceitunas 8) (conDescripcionMejorada (Capa Queso Prepizza))))
-- cantidadDeAceitunas (juntarAceitunas (Aceitunas 9) (juntarAceitunas (Aceitunas 8) (juntarAceitunas Queso (conDescripcionMejorada Prepizza))))
-- cantidadDeAceitunas (juntarAceitunas (Aceitunas 9) (juntarAceitunas (Aceitunas 8) (juntarAceitunas Queso Prepizza)))
-- cantidadDeAceitunas (juntarAceitunas (Aceitunas 9) (juntarAceitunas (Aceitunas 8) (Capa Queso Prepizza)))
-- cantidadDeAceitunas (juntarAceitunas (Aceitunas 9) (Capa (Aceitunas 8) (Capa Queso Prepizza)))
-- cantidadDeAceitunas (Capa (Aceitunas 9 + 8) (Capa Queso Prepizza)))
-- cantidadDeAceitunas (Capa (Aceitunas 17) (Capa Queso Prepizza)))
-- doCantidadDeAceitunas (Aceitunas 17) + cantidadDeAceitunas (Capa Queso Prepizza))
-- 17 + doCantidadDeAceitunas Queso + cantidadDeAceitunas Prepizza
-- 17 + 0 + cantidadDeAceitunas Prepizza
-- 17 + 0 + 0
-- 17
--------------------------------------------- Seccion 2 ---------------------------------------------
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
juntarPlanillas Fin p = p
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


-- Ejercicio 4
-- Demostrar:

-- a) para todo p :: Planilla. largoDePlanilla (juntarPlanillas Fin p) = largoDePlanilla Fin + largoDePlanilla p

-- Lado izq
-- largoDePlanilla (juntarPlanillas Fin p)
-- largoDePlanilla p

-- Lado der
-- largoDePlanilla Fin + largoDePlanilla p
-- 0 + largoDePlanilla p
-- largoDePlanilla p

-- b) para todo p :: Planilla. largoDePlanilla (juntarPlanillas (Registro "Edsger" Fin) p) = largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla p

-- Lado izq
-- largoDePlanilla (juntarPlanillas (Registro "Edsger" Fin) p)
-- largoDePlanilla (Registro "Edsger" (juntarPlanillas Fin p))
-- largoDePlanilla (Registro "Edsger" p)
-- 1 + largoDePlanilla p

-- Lado der
-- largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla p
-- 1 + largoDePlanilla Fin + largoDePlanilla p
-- 1 + 0 + largoDePlanilla p
-- 1 + largoDePlanilla p

-- c) para todo p :: Planilla
-- largoDePlanilla (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin )) p) = largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin )) + largoDePlanilla p

-- Lado izq
-- largoDePlanilla (juntarPlanillas (Registro "Alan" (Registro "Edsger" Fin )) p)
-- largoDePlanilla (Registro "Alan" (juntarPlanillas (Registro "Edsger" Fin ) p))
-- largoDePlanilla (Registro "Alan" (Registro "Edsger" (juntarPlanillas Fin p)))
-- largoDePlanilla (Registro "Alan" (Registro "Edsger" p))
-- 1 + largoDePlanilla (Registro "Edsger" p)
-- 1 + 1 + largoDePlanilla p
-- 2 + largoDePlanilla p

-- Lado der
-- largoDePlanilla (Registro "Alan" (Registro "Edsger" Fin)) + largoDePlanilla p
-- 1 + largoDePlanilla (Registro "Edsger" Fin) + largoDePlanilla p
-- 1 + 1 + largoDePlanilla Fin + largoDePlanilla p
-- 1 + 1 + 0 + largoDePlanilla p
-- 2 + largoDePlanilla p

-- Ejercicio 5
-- Demostrar:

-- a) largoDePlanilla (planillaDeIntegrantes (Becario "Alan")) = cantidadDeIntegrantes (Becario "Alan")

-- lado izq
-- largoDePlanilla (planillaDeIntegrantes (Becario "Alan"))
-- largoDePlanilla (Registro "Alan" Fin)
-- 1 + largoDePlanilla Fin
-- 1 + 0
-- 1

-- lado der
-- cantidadDeIntegrantes (Becario "Alan")
-- 1

-- d) largoDePlanilla (planillaDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen")))
--      = cantidadDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))

-- lado izq
-- largoDePlanilla (planillaDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen")))
-- largoDePlanilla (Registro "Alonzo" (juntarPlanillas (planillaDeIntegrantes (Becario "Alan")) (juntarPlanillas (planillaDeIntegrantes (Becario "Alfred")) (planillaDeIntegrantes (Becario "Stephen")))))
-- largoDePlanilla (Registro "Alonzo" (juntarPlanilla (Registro "Alan" Fin) (juntarPlanillas (Registro "Alfred" Fin) (Registro "Stephen" Fin) )))
-- largoDePlanilla (Registro "Alonzo" (juntarPlanilla (Registro "Alan" Fin) (Registro "Alfred" (juntarPlanillas Fin (Registro "Stephen" Fin) )))
-- largoDePlanilla (Registro "Alonzo" (juntarPlanilla (Registro "Alan" Fin) (Registro "Alfred" (Registro "Stephen" Fin)) ))
-- largoDePlanilla (Registro "Alonzo" (Registro "Alan" (juntarPlanilla Fin (Registro "Alfred" (Registro "Stephen" Fin) ))))
-- largoDePlanilla (Registro "Alonzo" (Registro "Alan" (Registro "Alfred" (Registro "Stephen" Fin))))
-- 1 + largoDePlanilla (Registro "Alan" (Registro "Alfred" (Registro "Stephen" Fin)))
-- 1 + 1 + largoDePlanilla (Registro "Alfred" (Registro "Stephen" Fin))
-- 1 + 1 + 1 + largoDePlanilla (Registro "Stephen" Fin)
-- 1 + 1 + 1 + 1 + largoDePlanilla Fin
-- 1 + 1 + 1 + 1 + 0
-- 4

-- lado der
-- cantidadDeIntegrantes (Investigador "Alonzo" (Becario "Alan") (Becario "Alfred") (Becario "Stephen"))
-- 1 + cantidadDeIntegrantes (Becario "Alan") + cantidadDeIntegrantes (Becario "Alfred") + cantidadDeIntegrantes (Becario "Stephen")
-- 1 + 1 + cantidadDeIntegrantes (Becario "Alfred") + cantidadDeIntegrantes (Becario "Stephen")
-- 1 + 1 + 1 + cantidadDeIntegrantes (Becario "Stephen")
-- 1 + 1 + 1 + 1
-- 4
--------------------------------------------- Seccion 3 ---------------------------------------------
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