--------------------------------------------------------------------------
																		--	
data Tree a = EmptyT | NodeT a ( Tree a ) ( Tree a )  deriving Show		--
																		--
--------------------------------------------------------------------------

arbolString :: Tree String
arbolString = NodeT "a" (NodeT "bb" (EmptyT) (EmptyT)) (NodeT "ccc" (NodeT "dddd" (EmptyT) (NodeT "eee" (EmptyT) (EmptyT))) (EmptyT)) 


arbol0 :: Tree Int
arbol0 = EmptyT

arbol2 :: Tree Int
arbol2 = NodeT 1
            (NodeT 2
                (NodeT 3
                    (NodeT 4 (EmptyT) (EmptyT)) 
                    (EmptyT))
                (EmptyT)) 
            (NodeT 5
                (NodeT 6 (EmptyT) (EmptyT))
                (NodeT 7 (EmptyT) (EmptyT)))
--
---           <-  1 -> 
---       <-  2      <- 5 ->
---    <- 3         6       7
---   4
---
---


sumarT :: Tree Int -> Int
--Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT EmptyT = 0
sumarT (NodeT x ti td ) = x + sumarT ti + sumarT td 

sizeT :: Tree a -> Int
--Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
--en inglés).
sizeT EmptyT = 0
sizeT (NodeT x ti td ) = 1 + sizeT ti + sizeT td 

mapDobleT :: Tree Int -> Tree Int
--Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT x ti td) = NodeT (x *2) (mapDobleT ti )  (mapDobleT td )
-----------------------------------------------------------------------

mapLongitudT :: Tree String -> Tree Int
--Dado un árbol de palabras devuelve un árbol con la longitud de cada palabra.
mapLongitudT EmptyT = EmptyT
mapLongitudT (NodeT x ti td) = NodeT (longitud x)  (mapLongitudT ti) (mapLongitudT td)


longitud :: [a] -> Int
--Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
--de elementos que posee.
longitud [] = 0
--longitud ls = 1 + longitud (tail ls)
longitud (a:xs) = 1 + longitud xs
-----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

perteneceT :: Eq a => a -> Tree a -> Bool
--Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
--árbol.
perteneceT a EmptyT = False
perteneceT a (NodeT x ti td) = (a == x) ||(perteneceT a ti) || (perteneceT a td) 

--------------------------------------------------------------------------------------------------

--arbol1=  NodeT 51(NodeT 12(NodeT 1 EmptyT EmptyT)



aparicionesT :: Eq a => a -> Tree a -> Int
--Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
--iguales a e.
aparicionesT e EmptyT = 0
aparicionesT e (NodeT x ti td ) = if (e == x)
									then (aparicionesT e ti) + (aparicionesT e td)	+1
									else (aparicionesT e ti) + (aparicionesT e td)							


countLeaves :: Tree a -> Int
--Dado un árbol devuelve su cantidad de hojas.
--Nota: una hoja (leaf en inglés) es un nodo que no tiene hijos.
countLeaves EmptyT = 0
countLeaves (NodeT x ti td ) = if esNodoConHijos (NodeT x ti td ) 
								then (countLeaves ti) + (countLeaves td)
								else 1
--------------------------------------------------------------------------------
esNodoConHijos :: Tree a -> Bool
esNodoConHijos (NodeT x EmptyT EmptyT )= False
esNodoConHijos (NodeT x ti td) = True

-------------------------------------------------------------------------
leaves :: Tree a -> [a]
--Dado un árbol devuelve los elementos que se encuentran en sus hojas.
leaves EmptyT = []
leaves (NodeT x EmptyT EmptyT) = [x]
leaves (NodeT x ti td) = leaves ti ++ leaves td 

-------------------------------------------------------------------------

heightT :: Tree a -> Int
--Dado un árbol devuelve su altura.
--Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
--de niveles del árbol1. La altura de un árbol vacío es cero y la de una hoja es 1.
heightT EmptyT = 0
heightT (NodeT x EmptyT EmptyT ) = 1
heightT (NodeT x ti td ) = max (1 + (heightT ti)) (1 + (heightT td))   

------------------------------------------------------------------------

countNotLeaves :: Tree a -> Int
--Dado un árbol devuelve el número de nodos que no son hojas. ¿Cómo podría resolverla sin
--utilizar recursión explícita? Primero defínala con recursión explícita y después sin ella.
countNotLeaves EmptyT = 0
countNotLeaves (NodeT x EmptyT EmptyT) = 1
countNotLeaves (NodeT x ti td ) = if esNodoConHijos (NodeT x ti td )
								  then 1 + (countNotLeaves ti) + (countNotLeaves td )
								  else 0 


----------------------------------------------------------------------------------------
mirrorT :: Tree a -> Tree a
--Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
--en cada nodo del árbol.
mirrorT EmptyT = EmptyT
mirrorT (NodeT x ti td ) = NodeT x (mirrorT td ) (mirrorT ti )

-----------------------------------------------------------------------------------------


arbol1= NodeT 1 (EmptyT) ( NodeT 2 (NodeT 3 (EmptyT) (EmptyT)) (NodeT 4 (EmptyT) (EmptyT)))


listInOrder :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
--Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
--y luego los elementos del hijo derecho.
listInOrder EmptyT = []
listInOrder (NodeT x ti td )= listInOrder ti ++ [x] ++ listInOrder td



------------------------------------------------------------------------------------------
listPreOrder :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo pre-order.
--Nota: En el modo pre-order primero se procesa la raiz, luego los elementos del hijo izquierdo,
--a continuación los elementos del hijo derecho. 
listPreOrder EmptyT = []
listPreOrder (NodeT x ti td) = [x] ++ listPreOrder ti ++ listPreOrder td 

