module Library where
import PdePreludat

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto { 
  ambientes :: Number,
  superficie :: Number,
  precio :: Number,
  barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
    mail :: Mail,
    busquedas :: [Busqueda]
}

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) =
  (ordenarSegun criterio . filter (not . criterio x)) xs ++
  [x] ++
  (ordenarSegun criterio . filter (criterio x)) xs

between cotaInferior cotaSuperior valor =
  valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
  Depto 3 80 7500 "Palermo", 
  Depto 1 45 3500 "Villa Urquiza", 
  Depto 2 50 5000 "Palermo", 
  Depto 1 45 5500 "Recoleta"]



-- 1
---- a

mayor :: (Ord a, Ord b) => (a -> b) -> a -> a -> Bool
mayor funcion elemento1 elemento2 =  funcion elemento1 > funcion elemento2

menor :: (Ord a, Ord b) => (a -> b) -> a -> a -> Bool
menor funcion elemento1 elemento2 =  funcion elemento1 < funcion elemento2 

---- b
{- 
   ordenarSegun (menor length) ["villavicencio", "yoda", "captus", "caja"] 
   ->["caja","yoda","captus","villavicencio"] 
   
   ordenarSegun (mayor length) ["villavicencio", "yoda", "captus", "caja"] 
   ->["villavicencio","captus","caja","yoda"]
-}

-- 2
---- a

ubicadoEn :: [Barrio] -> Requisito
ubicadoEn barriosInteres depto = any (== barrio depto) barriosInteres

---- b

cumpleRango :: (Depto -> Number) -> Number -> Number -> Requisito
cumpleRango criterio mayorAx menorAx depto = between mayorAx menorAx (criterio depto)

--3 
---- a

req = Requisito{
  ambientes = 1,
  superficie = 1,
  precio = 1,
  barrio = "palermo"
} deriving (Show, Eq)

cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda depto busqueda = all ( cumpleRequisito depto) busqueda

cumpleRequisito :: Depto -> Requisito -> Bool
cumpleRequisito depto requisito = requisito depto 