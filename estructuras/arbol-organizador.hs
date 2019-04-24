import Practica1


data Tree a = EmptyT | NodeT a ( Tree a ) (Tree a ) deriving (Show)

type Software = String

data Autor = Admin String | Dev String deriving (Show, Eq)

data Organizador = Agregar Software [Autor] Organizador | Vacio deriving (Show)

autor1 = Admin "Zuckerberg"
autor2 = Admin "Bill Gates"
autor3 = Admin "Steve Jobs"
autor4 = Dev "Jorge Sanchez"
autor5 = Dev "Fidel Castro"

organizador0 = Vacio
organizador1 = (Agregar "facebook" [autor1] (Agregar "microsoft" [autor2,autor4] (Agregar "apple" [autor3] (Vacio))))
organizador2 = (Agregar "goolge" [autor1,autor5,autor3,autor4,autor2] (Agregar "amazon" [autor4,autor5] (Vacio))) 

pares :: Organizador -> [(Software, Int)]
--dado un organizador, denota el conjunto de pares programa y cantidad de autores que existen.
pares Vacio = []
pares (Agregar soft lsAutor organizador) = (soft, (longitud lsAutor)) : pares organizador

enComun :: Autor -> Autor -> Organizador -> [Software]
--dados dos autores y un organizador, denota el conjunto de aquellos programas en los que ambos participaron.
enComun a1 a2 Vacio = []
enComun a1 a2 (Agregar soft lsAutor org) = if (elem a1 lsAutor) && (elem a2 lsAutor)
    then soft : enComun a1 a2 org 
    else enComun a1 a2 org 

filtrar :: [Autor] -> Organizador -> Organizador
--dado un conjunto de autores y un organizador, elimina esos autores de cada software.
filtrar lsAutor Vacio = Vacio 
filtrar xs (Agregar soft lsAutor org) = (Agregar soft (diferencia lsAutor xs) (filtrar xs org))


losAdmin :: Organizador -> [Autor]
--denota una lista con todos los administradores, sin elementos repetidos.
losAdmin Vacio = []
losAdmin (Agregar soft lsAutor org) = (admins lsAutor) ++ (losAdmin org)
-- OJO! Este no se podia resolver usando (:) en lugar de ++, error de tipo al hacer [] : []

admins :: [Autor] -> [Autor]
admins [] = []
admins (a:autores) = if esAdmin a 
    then a : admins autores 
    else admins autores 

esAdmin :: Autor -> Bool
esAdmin (Admin name) = True 
esAdmin _ = False


ordenados :: Organizador -> [Software]
--dado un organizador, denota la lista de programas ordenados de menor a mayor por cantidad de autores.
ordenados Vacio = []
ordenados org = [softConMenorAutores org] 
                        ++ ordenados (filtrarSoftware (softConMenorAutores org) org) 
-- filtrarElemento no me sirve para resolver este caso, xq recibe a y [a], no a y <tipo algebraico>

filtrarSoftware :: Software -> Organizador -> Organizador
filtrarSoftware soft Vacio = Vacio
filtrarSoftware soft (Agregar s a org) = if soft == s 
    then filtrarSoftware soft org -- Podria haber sido parcial y en lugar de esto devolver directo el org
    else (Agregar s a (filtrarSoftware soft org)) 

softConMenorAutores :: Organizador -> Software
softConMenorAutores (Agregar soft lsAutor Vacio) = soft
softConMenorAutores (Agregar soft lsAutor org) = if (longitud lsAutor) < (longitud (softConMenorAutores' org))
    then soft 
    else softConMenorAutores org

softConMenorAutores' :: Organizador -> [Autor]
softConMenorAutores' (Agregar soft lsAutor Vacio) = lsAutor
softConMenorAutores' (Agregar soft lsAutor org) = if (longitud lsAutor) < (longitud (softConMenorAutores' org)) 
    then lsAutor
    else softConMenorAutores' org


