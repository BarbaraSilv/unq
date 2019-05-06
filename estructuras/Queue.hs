
module Queue where
import Stack

data Queue a = Q (Stack a) (Stack a) Int deriving Show
-- Inv. de representacion: Explicacion de por que el Queue esta hecho con dos Stack
-- Int es la longitud del Queue


--O(1)
emptyQ :: Queue a --Crea una cola vacía.
emptyQ = Q []


--O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ [] = True
isEmptyQ _ = False


--O(n)
enqueue :: a -> Queue a -> Queue a
enqueue x (Q xs n) = Q (xs++[x])


--O(1)
firstQ :: Queue a -> a
firstQ (Q (x:xs) n) = x



--O(1)--
dequeue :: Queue a -> Queue a
dequeue (Q x:xs n) = Q xs



-- O(1)
lenQ :: Queue a -> Int -- Longitud
lenQ (Q _ n) = n



--Implemente la interfaz de Queue pero en lugar de una lista utilice dos stack.
--La estructura funciona de la siguiente manera. Llamemos a una de las stack fs (front stack) y a la otra bs(back stack). 
--Quitaremos elementos a través de fs y agregaremos a través de bs, pero todas las operaciones deben garantizar el siguiente invariante de representación: 
--Si fs se encuentra vacía, entonces la cola se encuentra vacía. ¿Qué ventaja tiene esta representación de Queue con respecto a la de listas?
