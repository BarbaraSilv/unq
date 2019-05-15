
import MapConRep 
module Celda where

data Color = Azul | Negro | Rojo | Verde deriving Show
data Celda = MkC (Map Color Int) deriving Show

--Inv Rep: existe una clave para cada color existente
-- el valor asociado al color es un positivo
-- <??> cual es la clave y cual es el valor???

celdaVacia :: Celda 
celdaVacia = MkC (emptyM)

poner :: Color -> Celda -> Celda 
poner color MkC (map) = MkC ( assocM map color ¿v? )

sacar :: Color -> Celda -> Celda 
--Prec: existe
sacar color MkC map = MkC ( deleteM map color )

nroBolitas :: Color -> Celda -> Int 



hayBolitas :: Color -> Celda -> Int 


