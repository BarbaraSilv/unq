module Set where 
import Practica1

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving (Show)

data Set a = Set [a] deriving (Show) -- Esta es la representacion con su constructor abstracto
-- Invariantes de representacion == condiciones
-- Inv. de representacion: el Set no tiene repetidos
-- En ningun caso le puedo pasar una lista con repetidos

-- set0 :: Set a
-- set0 = Set [1]

-- Pslabras en minuscula son parametros, palabras en mayuscula son constructores o tipos algebraicos

-- la restricción Eq aparece en toda la interfaz se utilice o no en todas las operacionesde esta implementación, 
--pero para mantener una interfaz común entre distintas posibles implementaciones estamos obligados a escribir así los tipos.

-- O(1)
emptyS :: Set a
--Crea Set vacio
emptyS = Set []



-- O(n) 
-- addS :: Eq a => a -> Set a -> Set a 
-- --Dados un elemento y un conjunto, agrega el elemento al conjunto. OJO! Si tuviera el inv. no repetidos, si intento agregar un repetido no lo va a hacer
-- addS x (Set xs) = 
--     if elem x xs 
--         then Set xs 
--         else Set (x:xs)

--O(1) Si hago cons al principio de la lista, es constante
addS :: Eq a => a -> Set a -> Set a 
addS x (Set xs) = S (x:xs)


-- O(n)
belongs :: Eq a => a -> Set a -> Bool 
--Pertenece 
belongs x (Set xs) = elem x xs

-- O(n)
sizeS :: Eq a => Set a -> Int 
sizeS (Set xs) = longitud xs

-- O(n)
removeS :: Eq a => a ->  Set a -> Set a
removeS x (Set xs) = Set (remove x xs)

-- O(n)
remove :: Eq a => a -> [a] -> [a]
remove c [] = []
remove c (x:xs) =
    if c == x then xs else x: remove c xs 


-- O(n) Concatenar es O(n)
unionS :: Eq a => Set a ->  Set a -> Set a
unionS (Set xs) (Set ys) = Set (xs ++ ys)

--O(n2) Tiene que hacer pertenece por cada elem. y despues la recursion del interseccion mismo
intersectionS :: Eq a => Set a -> Set a -> Set a
intersectionS (Set xs) (Set ys) = Set (interseccion xs ys)


--O(1)
setToList :: Eq a => Set a -> [a]
setToList (Set xs) = sacarRepetidos xs

sacarRepetidos [] = []
sacarRepetidos (x:xs) = agregar x (sacarRepetidos xs)

agregar x xs = if elem x xs then xs else x:xs

--O(1)
lenS :: Stack a -> Int 
lenS (S _ int _) = int

--maximoC :: Ord a => Set a -> a --Devuelve el máximo elemento en un conjunto 
--1. Implementar la variante que recorre la estructura buscando el máximo
--2. Implementar otra variante que no tenga que hacer un recorrido.
--Para hacer (2) Hay que resolver el anexo.