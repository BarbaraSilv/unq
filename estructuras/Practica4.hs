
import Set (emptyS, addS, belongs, sizeS, removeS, unionS, intersectionS, setToList)
import Queue (emptyQ, isEmptyQ, enqueue, dequeue, firstQ, lenQ) --OJO!!! En la practica enqueue se llama queue!!!
import Stack (emptyStack, isEmptyStack, push, pop, top, maxS) --OJO!! En la practica emptyStack y isEmptyStack son emptyS y isEmptyS
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
isEmptyS (St xs) = null (setToList (St xs))




--O(n2)
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
--Devuelve la lista que pertenecen al Set 
losQuePertenecen ls (St xs) = interseccion ls (setToList (St xs))




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


--O(1)
largoQ :: Queue a -> Int -- Cuenta la cantidad de elementos de la cola.
largoQ (Q fs bs n) = lenQ (Q fs bs n)


--O(1)
atender :: Queue String -> [String] -- Dada una cola de nombres de personas, devuelve la lista de los mismos,
--donde el orden de la lista es el de la cola.
atender (Q fs bs n) = queueToList (Q fs bs n)

--O(n2) --queueToList O(n), dequeue O(n)
queueToList :: Queue a -> [a]
queueToList emptyQ = []
queueToList (Q fs bs n) = firstQ :  queueToList (dequeue (Q fs bs n))


--O(n2) enqueue y dequeue son O(n), unirQ es O(n), en total, ¿O(n3)?
unirQ :: Queue a -> Queue a -> Queue a -- Inserta todos los elementos de la segunda cola en la primera.
unirQ (Q fs1 bs1 n1) emptyQ = (Q fs1 bs1 n1)
unirQ (Q fs1 bs1 n1) (Q fs2 bs2 n2) = 
    enqueue ( 
        (firstQ (Q fs2 bs2 n2)) 
        (unirQ (Q fs1 bs1 n1) (dequeue (Q fs2 bs2 n2) )) )
    


-- ////////////////////////////////////////////////////////////////////
-- ////////////////////////////////////////////////////////////////////
-- //////////////////////////////////// STACK 
-- ////////////////////////////////////////////////////////////////////
-- ////////////////////////////////////////////////////////////////////




--O(n) 
apilar :: [a] -> Stack a -- Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar [] = emptyStack
apilar (x:xs) (S ys n lsMax) = push x (apilar xs (S ys n lsMax) ) 

-- ////////////// IMPORTANTE: HACER ESTO NO VA MAS!!!!!!!!!!!!1
-- --O(n) n cantidad de elem. en el stack
-- desapilar :: Stack a -> [a] -- Dada una pila devuelve una lista sin alterar el orden de los elementos.
-- desapilar (S []) = []
-- desapilar (S (x:xs)) = x : desapilar (S xs)

--O(n2) recursion de desapilar + pop
desapilar :: Stack a -> [a] 
desapilar emptyStack = []
desapilar (S ys n lsMax) = top (S ys n lsMax) : desapilar (pop (S ys n lsMax))

--O(n2) 
treeToStack :: Tree a -> Stack a -- Dado un árbol devuelve una pila con los elementos apilados in order.
treeToStack tree =  apilar (listInOrder tree)
--listInOrder (NodeT x ti td) = (listInOrder ti) ++ [x] ++ (listInOrder td)



balanceado :: String -> Bool 
-- Toma un string que representa una expresión aritmética, por ejemplo ”(2 + 3)∗2”,
--y verifica que la cantidad de paréntesis que abren se corresponda con los que cierran. 
--Para hacer esto utilice un stack. Cada vez que encuentra un paréntesis que abre, lo apila. 
--Si encuentra un paréntesis que cierra, desapila un elemento. 
--Si al terminar de recorrer el string se desapilaron tantos elementos como los que se apilaron, ni más ni menos, 
--entonces los paréntesis están balaceados. 
--Pista: recorra una stack pasada por parámetro a una subtarea que devuelve un booleano, 
--que indifica si los parentesis están balanceados.
balanceado str = emptyStack == stringToStack str 

-- UN STRING ES UNA [CHAR] !!!!!!!!
-- "_(_"
-- "_)_"
-- "_(x)_"

stringToStack :: String -> Stack 
stringToStack (x:xs) = if x=='('
    then push x (stringToStack xs)
    else if x==')'
        then pop stringToStack
        else stringToStack


balanceadoConContar :: String -> Bool
balanceadoConContar str = ((contarParentesis str)) `mod` 2) == 0

contarLeftParentesis :: String -> Int
contarLeftParentesis (x:xs) = if x=='(' 
    then 1 + contarLeftParentesis xs 
    else contarLeftParentesis xs 

contarRightParentesis :: String -> Int
contarRightParentesis [] = 0
contarRightParentesis (x:xs) = if x==')'
    then 1 + contarRightParentesis xs 
    else contarRightParentesis xs 
