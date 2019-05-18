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
assocM (M xs) key value = (M (key, value):xs) -- se puede hacer esto? SI

--O(n)
lookupM :: Eq k =>  Map k v -> k -> Maybe v
--Buscar valor con clave
lookupM (M []) _ = Nothing
lookupM (M ((k,v):xs)) key = if k==key -- se puede hacer esto? SI
    then Just v

-- --O(n)
deleteM :: Eq k =>  Map k v -> k -> Map k v
--Borrar valor con clave. Prec: Existe -- <??> Como es este?????
-- porque supongamos que tengo el siguiente map
-- [ (1,a) (1,b) (1,c) (2,a) (3,a)] y le digo (deleteM 1). Cual estoy borrando? si quisiera borrar solo (1,c) 
-- por ejemplo? o (1,b)?
deleteM (M ((k,v):xs)) key = if k==key 
    then M xs
    else assocM x (deleteM xs key)

--O(n)
domM :: Map k v -> [k] 
--Denota lista de claves
domM (M []) = []
domM (M ((k,v):xs)) = k : domM (M xs)