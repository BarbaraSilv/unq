module MapConList where

data Map k v = M [k] [v] deriving Show
--Inv: k se representa con lista de claves, v con lista de valores
-- [k1,k2..kn] R [v1,v2..vn] (osea, k1 asoc. v1, k2 asoc. v2, etc)
--O(1)
emptyM :: Map k v 
--Devuelve map vacio
emptyM = M [] []

--Como es sin repetidos, me tengo que fijar si ya existe. Lo puedo hacer o a lo bruto o con un Set. Yo lo voy a hacer a lo bruto.
-- <??> Si inserto uno que ya existe, se sobreescribe el valor? o no se inserta?
--O(n)
assocM  :: Eq k =>  Map k v -> k -> v -> Map k v
--Agregar clave
assocM (M xs ys) key value = if elem key ys
    then (M xs ys) 
    else (M (key:xs) (value:ys))

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