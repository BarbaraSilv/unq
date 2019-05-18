module MapSinRep where

data Map k v = M [(k, v)] deriving Show

--O(1)
emptyM :: Map k v 
--Devuelve map vacio
emptyM = M []

--Como es sin repetidos, me tengo que fijar si ya existe. Lo puedo hacer o a lo bruto o con un Set. Yo lo voy a hacer a lo bruto.
-- Si inserto uno que ya existe, se sobreescribe el valor? SI

--O(n2)
assocM  :: Eq k =>  Map k v -> k -> v -> Map k v
--Agregar clave
assocM (M []) key value = M [(key, value)]
assocM (M xs) key value = if elem key (domM (M xs)) 
    then reemplazarValorM (M xs) key value
    else (M (key,value):xs)

--O(n)
reemplazarValorM :: Eq k => Map k v -> k v -> Map k v 
reemplazarValorM (M []) _ _ = emptyM
reemplazarValorM (M ((k,v):xs)) key value = if k==key 
    then (M (k,value):xs)
    else assocM' ((reemplazarValorM (M xs) key value) k v)

--O(1) Agregar (v,k) en un map, sin fijarse si ya existe
assocM'  :: Eq k =>  Map k v -> k -> v -> Map k v
assocM' (M xs) key value = assocM' (M ((key,value):xs)) 


--O(n)
lookupM :: Eq k =>  Map k v -> k -> Maybe v
--Buscar valor con clave
lookupM (M []) _ = Nothing
lookupM (M ((k,v):xs)) key = if k==key -- SI se puede hacer esto
    then Just v

-- --O(n)
deleteM :: Eq k =>  Map k v -> k -> Map k v
--Borrar valor con clave. Prec: Existe
-- deleteM (M []) _ = emptyM Prec.
deleteM (M (x:xs)) key = if (fst x)==key 
    then M xs
    else assocM x (deleteM xs key) -- <??> Habia otra forma de hacer este, pero no la copie



--O(n)
domM :: Map k v -> [k]
--Denota lista de claves
domM (M []) = []
domM (M ((k,v):xs)) = k : domM (M xs)