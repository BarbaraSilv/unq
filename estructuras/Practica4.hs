
import Set (emptyS, addS, belongs, sizeS, removeS, unionS, intersectionS, setToList)
import Queue (emptyQ, isEmptyQ, enqueue, dequeue, firstQ) --OJO!!! En la practica enqueue se llama queue!!!
import Stack (emptyStack, isEmptyStack, push, pop, top) --OJO!! En la practica emptyStack y isEmptyStack son emptyS y isEmptyS
import Practica3


-- ////////////////////////////////////////////////////////////////////
-- ////////////////////////////////////////////////////////////////////
-- //////////////////////////////////// SET 
-- ////////////////////////////////////////////////////////////////////
-- ////////////////////////////////////////////////////////////////////



-- (emptyS, addS, belongs, sizeS, removeS, unionS, intersectionS, setToList)
--   O(1),  O(1),  O(n),   O(n),   O(n),    O(n),     O(n2),      O(1)

-- belongs :: Eq a => a -> Set a -> Bool 


--O(1)
isEmptyS :: Set a -> Bool
--isEmptyS (Set xs) = null xs
isEmptyS (Set xs) = null (setToList (Set xs))




--O(n2)
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
--Devuelve la lista que pertenecen al Set 
losQuePertenecen ls (Set xs) = interseccion ls (setToList (Set xs))




-- O(n)
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (listToSet xs)




listToSet :: [a] -> Set a
listToSet [] = emptyS
listToSet (x:xs) = addS x (listToSet xs)




--O(n) n cantidad de nodos
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT s ti td) = UnionS s (UnionS (unirTodos ti) (unitTodos td))

-- NO USAR SET, NI USAR S!!!! MUY IMPORTANTE SINO ME VA A IR RECONTRA MAL
-- HACER TODO LO POSIBLE POR NO USAR NI SET NI S NI NINGUN CONSTRUCTOR, PORQUE SINO ME CONVIERTO EN EL IMPLEMENTADOR TRUCHO DE UN SET

-- ////////////////////////////////////////////////////////////////////
-- ////////////////////////////////////////////////////////////////////
-- //////////////////////////////////// QUEUE
-- ////////////////////////////////////////////////////////////////////
-- ////////////////////////////////////////////////////////////////////




--O(n)
largoQ :: Queue a -> Int -- Cuenta la cantidad de elementos de la cola.
largoQ (Q []) = 0
largoQ (Q (x:xs)) = 1 + largoQ (Q xs)


--O(1)
atender :: Queue String -> [String] -- Dada una cola de nombres de personas, devuelve la lista de los mismos,donde el orden de la lista es el de la cola.
atender (Q xs) = xs


--O(n)
unirQ :: Queue a -> Queue a -> Queue a -- Inserta todos los elementos de la segunda cola en la primera.
unirQ (Q xs) (Q ys) = Q (xs ++ ys)



-- ////////////////////////////////////////////////////////////////////
-- ////////////////////////////////////////////////////////////////////
-- //////////////////////////////////// STACK 
-- ////////////////////////////////////////////////////////////////////
-- ////////////////////////////////////////////////////////////////////




--O(n) 
apilar :: [a] -> Stack a -- Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar [] = emptyStack
apilar (x:xs) = push x (apilar xs)
Stack [1,2,3] ===>>> [1,2,3]


--O(n) n cantidad de elem. en el stack
desapilar :: Stack a -> [a] -- Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar (S []) = []
desapilar (S (x:xs)) = x : desapilar (S xs)



--O(n) n cantidad de nodos
treeToStack :: Tree a -> Stack a -- Dado un árbol devuelve una pila con los elementos apilados in order.
treeToStack tree = S (listInOrder tree) 
--listInOrder (NodeT x ti td) = (listInOrder ti) ++ [x] ++ (listInOrder td)



balanceado :: String -> Bool 
-- Toma un string que representa una expresión aritmética, por ejemplo ”(2 + 3)∗2”,
--y verifica que la cantidad de paréntesis que abren se corresponda con los que cierran. 
--Para hacer esto utilice un stack. Cada vez que encuentra un paréntesis que abre, lo apila. 
--Si encuentra un paréntesis que cierra, desapila un elemento. 
--Si al terminar de recorrer el string se desapilaron tantos elementos como los que se apilaron, ni más ni menos, 
--entonces los paréntesis están balaceados. 
--Pista: recorra una stack pasada por parámetro a una subtarea que devuelve un booleano, que indifica si los parentesis están balanceados.


-- /////////////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////////////
-- /////////////////////////////// A N E X O
-- /////////////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////////////

-- <??> ALGUIEN ME PUEDE EXPLICAR QUE CARAJO ES UN MAYBE????