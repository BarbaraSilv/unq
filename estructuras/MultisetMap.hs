module MultisetMap where 
import MapConRep

fromJust :: Maybe a -> a 
fromJust Nothing = Error
fromJust (Just x) = x

data Multiset a = MS (Map Int a) Int deriving Show 
-- Inv. rep: Set pero con repetidos, cada elemento representado como un par (Int a), donde Int es la cantidad
-- de apariciones del elemento
-- Segundo Int es la primera clave usada



--O(1)
emptyMS :: Multiset a 
emptyMS = MS emptyM 0

--O() -- Cuando agrego un elemento, me tengo que fijar si ese elemento ya existe en el
-- MS para actualizar las ocurrencias
addMS :: Ord a => a -> Multiset a -> Multiset a 
addMS x (MS map n) = MS (assocM map n x) n+1
-- ESTO FUNCIONAaaaahh (te lo explico lautaro, acordate)

--O()
ocurrencesMS :: Ord a => a -> Multiset a -> Int 
ocurrencesMS e (MS map) = last (lookupM map e) 
-- 2. Como convierto un Just v en v? usar fromJust

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
