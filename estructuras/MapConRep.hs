module MapConRep where

data Map k v = M [(k, v)] deriving Show
data Maybe a = Nothing | Just a deriving Show

--O(1)
emptyM :: Map k v 
--Devuelve map vacio
emptyM = M []

--O(1)
assocM  :: Eq k =>  Map k v -> k -> v -> Map k v
--Agregar clave
assocM (M xs) key value = (M (key, value):xs) -- <??> se puede hacer esto?

--O(n)
lookupM :: Eq k =>  Map k v -> k -> Maybe v
--Buscar valor con clave
lookupM (M []) _ = Nothing
lookupM (M ((k,v):xs)) key = if k==key -- <??> se puede hacer esto?
    then Just v

-- --O(n)
deleteM :: Eq k =>  Map k v -> k -> Map k v -- <??> Al querer testear me tira un error raro
--Borrar valor con clave. Prec: Existe
deleteM (M (x:xs)) key = if (fst x)==key 
    then M xs
    else assocM x (deleteM xs key)

--O(n)
domM :: Map k v -> [k] 
--Denota lista de claves
domM (M []) = []
domM (M ((k,v):xs)) = k : domM (M xs)