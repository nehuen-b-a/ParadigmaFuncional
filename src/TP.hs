module TP where
  
-- Se importa una implementación básica para la función show para las funciones,
-- de modo que en la consola se muestren como "<function>" en vez de fallar
-- por un error de tipos.
import Text.Show.Functions

----------------------
-- Código inicial
----------------------

ordenarPor :: Ord a => (b -> a) -> [b] -> [b]
ordenarPor ponderacion =
  foldl (\ordenada elemento -> filter ((< ponderacion elemento).ponderacion) ordenada
                                  ++ [elemento] ++ filter ((>= ponderacion elemento).ponderacion) ordenada) []

data Demonio = Demonio {
    deudores :: [String],
    subordinados :: [Demonio]
} deriving (Show, Eq)

----------------------------------------------
-- Definí tus tipos de datos y funciones aquí
----------------------------------------------

-- Punto 1

type Deseo = Humano -> Humano
type Atributo = Humano -> Int
type Carrera = String

data Humano = Humano {
    nombre :: String,
    felicidad :: Int,
    reconocimiento :: Int,
    deseos :: [Deseo]
} deriving (Show)

aplicarAtributo ::  Atributo -> Atributo -> Deseo
aplicarAtributo atributo1 atributo2 humano = Humano {
    nombre = nombre humano,
    felicidad = atributo1 humano, 
    reconocimiento = atributo2 humano, 
    deseos = deseos humano 
}

laPazMundial :: Deseo
laPazMundial = aplicarAtributo ((20*).felicidad) reconocimiento

recibirseDe :: Carrera -> Deseo
recibirseDe carrera = aplicarAtributo ((250+).felicidad) (((3*length carrera)+).reconocimiento)

serFamoso ::  Deseo
serFamoso = aplicarAtributo ((50+).(0*).felicidad) ((1000+).reconocimiento)

humanoDePrueba = Humano {
    nombre = "Pedro",
    felicidad = 100, 
    reconocimiento = 50, 
    deseos = [laPazMundial, recibirseDe "Ingeniería en Sistemas de Información", recibirseDe "Medicina", serFamoso] 
}

-- Punto 2

diferenciaFelicidadReconocimiento :: Humano -> Humano -> Int
diferenciaFelicidadReconocimiento humano1 humano2 = (felicidad humano2 - felicidad humano1) - (reconocimiento humano2 - reconocimiento humano1)

espiritualidad :: Humano -> Deseo -> Int
espiritualidad humano deseo = diferenciaFelicidadReconocimiento humano (deseo humano)


-- Punto 3

aplicarDeseo :: Humano -> Deseo -> Humano
aplicarDeseo humano deseo = deseo humano

mejoraFelicidadAlCumplirDeseos :: Humano -> Bool
mejoraFelicidadAlCumplirDeseos humano = ((> felicidad humano).felicidad.(foldl aplicarDeseo humano).deseos) humano


-- Punto 4

esDeseoTerrenal :: Humano -> Deseo -> Bool
esDeseoTerrenal humano = (<150).espiritualidad humano

deseosMasTerrenales :: Humano -> [Deseo] 
deseosMasTerrenales humano = filter (esDeseoTerrenal humano) (ordenarPor (espiritualidad humano) (deseos humano))

criterioMejorVersion :: Humano -> Deseo -> Int
criterioMejorVersion humano deseo = (felicidad.aplicarDeseo humano) deseo + (reconocimiento.aplicarDeseo humano) deseo

mejorVersion :: Humano -> Humano
mejorVersion humano = last (ordenarPor (criterioMejorVersion humano) (deseos humano)) humano


-- Punto 5a

tienePoderSobre :: Demonio -> Humano -> Bool
tienePoderSobre demonio humano = esDeudorDirecto humano demonio || esDeudorSubordinado humano demonio

esDeudorDirecto :: Humano -> Demonio -> Bool
esDeudorDirecto humano demonio = elem (nombre humano) (deudores demonio)

esDeudorSubordinado :: Humano -> Demonio -> Bool
esDeudorSubordinado humano demonio = any (esDeudorDirecto humano) (subordinados demonio)

-- Punto 5b

indirectosTienePoderSobre :: Demonio -> Humano -> Bool
indirectosTienePoderSobre  demonio humano = elem (nombre humano) (deudoresTotales demonio)

deudoresTotales :: Demonio -> [String] 
deudoresTotales demonio = (concatMap deudores (demoniosIndirectos demonio)) ++ deudores demonio

demoniosIndirectos :: Demonio -> [Demonio]
demoniosIndirectos demonio = concatMap demoniosIndirectos (subordinados demonio) ++ (subordinados demonio)


-- Punto 6

asignarDeudor :: Demonio -> Humano -> Demonio 
asignarDeudor demonio humano = Demonio { 
    deudores = deudores demonio ++ [nombre humano], 
    subordinados = subordinados demonio
}

tieneDeseosTerrenales :: Humano -> Bool
tieneDeseosTerrenales = not.null.deseosMasTerrenales 

ayudarSiLeConviene :: Humano -> Demonio -> (Humano, Demonio)
ayudarSiLeConviene humano demonio
    | not (tienePoderSobre demonio humano) &&  tieneDeseosTerrenales humano = (aplicarDeseo humano (head.deseosMasTerrenales $ humano), asignarDeudor demonio humano)
    | otherwise = (humano, demonio)