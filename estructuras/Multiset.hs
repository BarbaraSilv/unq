module Multiset where 

data Multiset a = MS [a] deriving Show 
-- Inv. rep: Set pero con repetidos, y para cada elemento puedo saber cuanto aparece c/u

--O(1)
emptyMS :: Multiset a 
emptyMS = MS []

--O(1)
addMS :: Ord a => a -> Multiset a -> Multiset a 
addMS x (MS (xs)) = MS (x:xs)

--O(n)
ocurrencesMS :: Ord a => a -> Multiset a -> Int 
ocurrencesMS _ (MS []) = 0
ocurrencesMS e (MS (x:xs)) = if e==x 
then 1 + ocurrencesMS e (MS xs)
else ocurrencesMS e (MS xs)

--O(n)? <??>
unionMS :: Ord a => Multiset a -> Multiset a -> Multiset a 
unionMS (MS (xs)) (MS (ys)) = MS (xs++ys)

--O(n2)
intersectionMS :: Ord a => Multiset a -> Multiset a -> Multiset a 
intersectionMS (MS xs) (MS ys) = MS (interseccion xs ys)

--O(?)
multisetToList :: Ord a => Multiset a -> [(Int,a)] 
multisetToList (MS (x:xs)) = 

-- <??> Si le paso este multiset, que deberia devolver? 
-- MS [a,b,c,a]
-- [(2,a),(1,b),(1,c)] o  [(2,a),(1,b),(1,c),(2,a)]


