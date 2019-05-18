module MapConList where

data Map k v = M [k] [v] deriving Show
--Inv: k se representa con lista de claves, v con lista de valores
-- [k1,k2..kn] R [v1,v2..vn] (osea, k1 asoc. v1, k2 asoc. v2, etc)
-- Ambas listas tienen la misma longitud

--O(1)
emptyM :: Map k v 
--Devuelve map vacio
emptyM = M [] []

--O(n2) Inserciones en el head de las listas
assocM  :: Eq k =>  Map k v -> k -> v -> Map k v
--Agregar clave. Si ya existe, sobreescribe
assocM (M xs ys) key value = if elem k xs 
    then (M xs ys)
    else M ((key:xs) (value:ys))

--O(n)
reemplazarValorM :: Eq k => Map k v -> k v -> Map k v 
reemplazarValorM (M [] []) _ _ = emptyM
reemplazarValorM (M (k:ks) (v:vs)) key value = if k==key 
    then (M (k:ks) (value:vs))
    else assocM' ((reemplazarValorM (M ks vs) key value) k v)

--O(1) Agregar (v,k) en un map, sin fijarse si ya existe
assocM'  :: Eq k =>  Map k v -> k -> v -> Map k v
assocM' (M xs ys) key value = assocM' (M (key:xs) (value:ys))



--O(n)
lookupM :: Eq k =>  Map k v -> k -> Maybe v
--Buscar valor con clave
lookupM (M [] []) _ = Nothing
lookupM (M (x:xs) (y:ys)) key = if x==key
    then Just y
    else lookupM (M xs ys) key 

--O(n)
deleteM :: Eq k =>  Map k v -> k -> Map k v
--Borrar valor con clave. Prec: Existe
deleteM (M (x:xs) (y:ys)) key = if x==key 
    then (M xs ys)
    else assocM (deleteM (M xs ys) key) x y

--O(1)
domM :: Map k v -> [k]
--Denota lista de claves
domM (M xs ys) = xs