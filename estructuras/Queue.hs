
module Queue where
import StackSimple

data Queue a = Q (Stack a) (Stack a) Int deriving Show
-- Inv. de representacion: Int es la longitud del Queue.
-- fs=front Stack, bs=back stack, quitaremos elementos a traves de fs y los agregaremos a traves de bs.
-- Si fs se encuentra vacia, entonces el Queue esta vacio.
-- fs y bs tienen mismos elementos pero ordenados al reves (orden O(n) amortizado (CON UN REVERSE NUEVO PARA STACK))
--Si hago enqueue y fs esta vacia, entonces agrego elem. en fs. Sino lo agrego en bs.


--O(1)
emptyQ :: Queue a --Crea una cola vacía.
emptyQ = (Q ( (S [] 0) (S [] 0) ) 0)


--O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q (S [] 0) bs) _ = True
isEmptyQ _ _ = False



--O(1) push O(1)
enqueue :: a -> Queue a -> Queue a
enqueue x (Q fs bs n) = if isEmptyStack fs
    then Q (push x fs) bs n+1
    else Q fs (push x bs) n+1


--O(1) amortizado 
dequeue :: Queue a -> Queue a
dequeue x (Q fs bs n) = if lenStack fs == 1
    then Q (pop (reverse' bs)) [] n-1 
    else Q (pop fs) bs n-1

-- Lo doy vuelta porque sino me esta sacando elementos como si fuera un stack, pero no es un stack,
-- es un queue

-- O(n^2)
reverse' :: Stack a -> Stack a
reverse' (S (xs)) = S (reverse'' xs)

-- O(n^2)
reverse'' :: [a] -> [a]
reverse'' [] = []
reverse'' (x:xs) = reverse'' xs ++ [x] 


--O(1) prec: Al menos un elemento
firstQ :: Queue a -> a
firstQ (Q fs bs n) = top fs


-- O(1)
lenQ :: Queue a -> Int -- Longitud
lenQ (Q fs bs n) = n
