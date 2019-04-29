module Set where 
import Practica1

data Set a = Set [a] deriving (Show)
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving (Show)


-- la restricción Eq aparece en toda la interfaz se utilice o no en todas las operacionesde esta implementación, 
--pero para mantener una interfaz común entre distintas posibles implementaciones estamos obligados a escribir así los tipos.

-- O(1)
emptyS :: Set a
--Crea Set vacio
emptyS = Set []

-- O(1)
addS :: Eq a => a -> Set a -> Set a 
--Dados un elemento y un conjunto, agrega el elemento al conjunto
addS x (Set xs) = Set (x:xs) 

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
remove :: a -> [a] -> [a]
remove c [] = []
remove c (x:xs) =
    if c == x then xs else x: remove c xs 


-- O(n) Concatenar es O(n)
unionS :: Eq a => Set a ->  Set a -> Set a
unionS (Set xs) (Set ys) = Set (xs ++ ys)

--O(n2) ? Tiene que hacer pertenece por cada elem. y despues la recursion del interseccion mismo
intersectionS :: Eq a => Set a -> Set a -> Set a
intesectionS (Set xs) (Set ys) = Set (interseccion xs ys)


--O(1)
setToList :: Eq a => Set a -> [a]
setToList (Set xs) = xs


