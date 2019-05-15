module MultisetMap where 
import MapConRep

data Multiset a = MS (Map Int a) deriving Show 
-- Inv. rep: Set pero con repetidos, cada elemento representado como un par (Int a), donde Int es la cantidad
-- de apariciones del elemento
-- <??> Cual es el key y cual es el value??

--O(1)
emptyMS :: Multiset a 
emptyMS = MS emptyM

--O() -- Cuando agrego un elemento, me tengo que fijar si ese elemento ya existe en el
-- MS para actualizar las ocurrencias
addMS :: Ord a => a -> Multiset a -> Multiset a 
addMS x (MS map) = assocM map x

--O()
ocurrencesMS :: Ord a => a -> Multiset a -> Int 
ocurrencesMS e (MS map) = last (lookupM map e) 
-- <??> 1. Resolver cual es el datatype de Multiset
-- 2. Como convierto un Just v en v?

--O()
unionMS :: Ord a => Multiset a -> Multiset a -> Multiset a 
unionMS (MS map1) (MS map2) = 

--O()
intersectionMS :: Ord a => Multiset a -> Multiset a -> Multiset a 
intersectionMS (MS map1) (MS map2) = 

--O(1)
multisetToList :: Ord a => Multiset a -> [(Int,a)] 
multisetToList (MS map) = 

-- <??> Si le paso este multiset, que deberia devolver? 
-- MS [a,b,c,a]
-- [(2,a),(1,b),(1,c)] o  [(2,a),(1,b),(1,c),(2,a)]
