module Set where 
import Practica1

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving (Show)

data Set a = St [a] [a] deriving (Show, Ord) -- Esta es la representacion con su constructor abstracto
-- Invariantes de representacion == condiciones
-- Inv. de representacion: el Set no tiene repetidos
-- En ningun caso le puedo pasar una lista con repetidos
-- el segundo [a] es una lista ordenada de los elementos de mayor a menor

-- set0 :: Set a
-- set0 = Set [1]

-- Pslabras en minuscula son parametros, palabras en mayuscula son constructores o tipos algebraicos

-- la restricción Eq aparece en toda la interfaz se utilice o no en todas las operacionesde esta implementación, 
--pero para mantener una interfaz común entre distintas posibles implementaciones estamos obligados a escribir así los tipos.

-- O(1)
emptyS :: Set a
--Crea Set vacio
emptyS = St [] []




--O(1) Si hago cons al principio de la lista, es constante. CASO SI PUDIERA TENER REPETIDOS
--addS :: Eq a => a -> Set a -> Set a 
--addS x (Set xs) = S (x:xs)

--O(n2) 
addS :: Eq, Ord a => a -> Set a -> Set a 
addS x (St xs lsMax) = 
     if elem x xs 
         then St xs (actualizarMax x lsMax)
         else St (x:xs)

--O(n)
actualizarMax :: Ord a => a -> [a] -> [a]
actualizarMax e [] = [e]
actualizarMax e xs = if e > head xs 
    then e : xs
    else x : actualizarMax e xs


-- O(n)
belongs :: Eq a => a -> Set a -> Bool 
--Pertenece 
belongs x (St xs lsMax) = elem x xs

-- O(n)
sizeS :: Eq a => Set a -> Int 
sizeS (St xs lsMax) = longitud xs

-- O(n)
removeS :: Eq a => a ->  St a -> Set a
removeS x (St xs lsMax) = St (remove x xs) (remove x lsMax)

-- O(n)
remove :: Eq a => a -> [a] -> [a]
remove c [] = []
remove c (x:xs) =
    if c == x then xs else x: remove c xs 


-- O(n2) Concatenar es O(n)
unionS :: Eq a => Set a ->  Set a -> Set a
unionS (St [] lsMax1) (St ys lsMax2) = emptyS
unionS (St xs lsMax1) (St (y:ys) lsMax2) = addS y (unionS (St xs lsMax1) (St ys lsMax2))


--O(n2) Tiene que hacer pertenece por cada elem. y despues la recursion del interseccion mismo
intersectionS :: Eq a => Set a -> Set a -> Set a
intersectionS (St xs lsMax1) (St ys lsMax2) = St (interseccion xs ys) (interseccion lsMax1 lsMax2)


--O(1)
setToList :: Eq a => Set a -> [a]
setToList (St xs lsMax) = xs

--maximoC :: Ord a => Set a -> a --Devuelve el máximo elemento en un conjunto 
--1. Implementar la variante que recorre la estructura buscando el máximo
--2. Implementar otra variante que no tenga que hacer un recorrido.

-- Version 2. O(1)
maximoC :: Ord a => Set a -> a 
maximoC (St xs lsMax) = head lsMax



