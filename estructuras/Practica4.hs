
import Set (emptyS, addS, belongs, sizeS, removeS, unionS, intersectionS, setToList)
import Queue (emptyQ, isEmptyQ, enqueue, dequeue, firstQ) --OJO!!! En la practica enqueue se llama queue!!!
import Stack (emptyStack, isEmptyStack, push, pop, top) --OJO!! En la practica emptyStack y isEmptyStack son emptyS y isEmptyS


-- ////////////////////////////////////////////////////////////////////
-- //////////////////////////////////// SET 
-- ////////////////////////////////////////////////////////////////////


-- (emptyS, addS, belongs, sizeS, removeS, unionS, intersectionS, setToList)
--   O(1),  O(1),  O(n),   O(n),   O(n),    O(n),     O(n2),      O(1)

-- belongs :: Eq a => a -> Set a -> Bool 


--O(n2)
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
--Devuelve la lista que pertenecen al Set 
losQuePertenecen [] _ = []
losQuePertenecen (x:xs) (Set ys) = if belongs x (Set ys) 
    then x : losQuePertenecen xs (Set ys)
    else losQuePertenecen xs (Set ys)


-- O(n)
sinRepetidos :: Eq a => [a] -> [a]
-- <??> Quita todos los elementos repetidos de la lista dada utilizando un Set como estructura auxiliar.
¿sinRepetidos xs = setToList (Set xs)?



--O(n)??
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = []
unirTodos (NodeT (Set xs) ti td) = Set (xs ++ unirTodos ti ++ unitTodos td)


-- ////////////////////////////////////////////////////////////////////
-- //////////////////////////////////// QUEUE

largoQ :: Queue a -> Int -- Cuenta la cantidad de elementos de la cola.

atender :: Queue String -> [String] -- Dada una cola de nombres de personas, devuelve la lista de los mismos,donde el orden de la lista es el de la cola.

unirQ :: Queue a -> Queue a -> Queue a -- Inserta todos los elementos de la segunda cola en la primera.

-- ////////////////////////////////////////////////////////////////////
-- //////////////////////////////////// STACK 

apilar :: [a] -> Stack a -- Dada una lista devuelve una pila sin alterar el orden de los elementos.

desapilar :: Stack a -> [a] -- Dada una pila devuelve una lista sin alterar el orden de los elementos.

treeToStack :: Tree a -> Stack a -- Dado un árbol devuelve una pila con los elementos apilados inorder.

balanceado :: String -> Bool -- Toma un string que representa una expresión aritmética, por ejemplo”(2 + 3)∗2”,y verifica que la cantidad de paréntesis 
--que abren se corresponda con los que cierran.Para hacer esto utilice una stack. Cada vez que encuentra un paréntesis que abre, loapila. Si encuentra un 
--paréntesis que cierra desapila un elemento. Si al terminar de recorrer el string se desapilaron tantos elementos como los que se apilaron, ni más ni menos, 
--entonces los paréntesis están balaceados. Pista: recorra una stack pasada porparámetro a una subtarea que devuelve un booleano, que indifica si los parentesis 
--están balanceados.
