
import MapSinRep 
module Celda where

data Color = Azul | Negro | Rojo | Verde deriving Show
data Celda = MkC (Map Color Int) deriving Show

--Inv Rep: existe una clave para cada color existente
-- el valor asociado al color es un positivo
-- Map guarda los 4 colores al mismo tiempo
-- Los colores son las claves

--O( eficiencia de assocM )
celdaVacia :: Celda 
celdaVacia = MkC (assocM (assocM (assocM (assocM (emptyM) Verde 0) Rojo 0) Negro 0) Azul 0) 

--O( eficiencia de lookupM o de assocM, el que sea menos eficiente )
poner :: Color -> Celda -> Celda 
poner color (MkC map) = MkC (assocM (map) color ((lookupM map color)+1))

--O(1)
sacar :: Color -> Celda -> Celda 
--Prec: existe
sacar color (MkC map) = MkC (assocM (map) color ((lookupM map color)-1))

--O(n)
nroBolitas :: Color -> Celda -> Int
nroBolitas color (MkC map) = (lookupM map color)


hayBolitas :: Color -> Celda -> Bool 
hayBolitas color (MkC map) = 

