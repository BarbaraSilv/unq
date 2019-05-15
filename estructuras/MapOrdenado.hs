module MapSinRep where

data Map k v = M [(k, v)] deriving Show
--Inv: ordenado de menor a mayor segun la clave

--O(1)
emptyM :: Map k v 
--Devuelve map vacio
emptyM = M []

--Como es sin repetidos, me tengo que fijar si ya existe. Lo puedo hacer o a lo bruto o con un Set. Yo lo voy a hacer a lo bruto.
-- <??> Si inserto uno que ya existe, se sobreescribe el valor? o no se inserta?

--O(n2) <??> es esto o O(n) amortizado?
assocM  :: Eq k =>  Map k v -> k -> v -> Map k v
--Agregar clave
assocM (M xs) key value = if elem key (domM (M xs)) 
    then (M xs) 
    else assocOrdenado (M xs) key value

--O(n2)
assocOrdenado :: Eq k => Map k v -> k -> v -> Map k v
assocOrdenado (M (x:xs)) key value = if key < (fst x) 
    then (M (key,value):x:xs)
    else assocM (assocOrdenado (M xs) key value) x

--O(n)
lookupM :: Eq k =>  Map k v -> k -> Maybe v
--Buscar valor con clave
lookupM (M []) _ = Nothing
lookupM (M ((k,v):xs)) key = if k==key -- <??> se puede hacer esto?
    then Just v

-- --O(n)
deleteM :: Eq k =>  Map k v -> k -> Map k v
--Borrar valor con clave. Prec: Existe
deleteM (M (x:xs)) key = if (fst x)==key 
    then M xs
    else assocM (deleteM xs key) x

--O(n)
domM :: Map k v -> [k]
Denota lista de claves
domM (M []) = []
domM (M ((k,v):xs)) = k : domM (M xs)