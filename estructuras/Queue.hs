
module Queue where

data Queue a = Q [a] deriving Show

queue1 :: Queue a
queue1 = Q [1,2,3,4,5]



--O(1)
emptyQ :: Queue a --Crea una cola vacía.
emptyQ = Q []


--O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ [] = True
isEmptyQ _ = False


--O(n)
enqueue :: a -> Queue a -> Queue a
enqueue x (Q xs) = Q (xs++[x])


--O(1)
firstQ :: Queue a -> a
firstQ (Q (x:xs)) = x



--O(1)
dequeue :: Queue a -> Queue a
dequeue (Q x:xs) = Q xs



-- O(1)
lenQ :: Queue a -> Int -- Longitud
lenQ (Queue []) = 0
lenQ (Queue (x:xs)) = 1 + lenQ (Queue xs)



