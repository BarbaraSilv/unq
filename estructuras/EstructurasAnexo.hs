
data Maybe a = Nothing | Just a --Por defecto en Haskell

--O(1)
headM :: [a] -> Maybe a --head pero total
headM [] = Nothing 
headM (x:xs) = Just x


--O(n)
lastM :: [a] -> Maybe a --last pero total
lastM [] = Nothing
lastM [x] = Just x 
lastM (x:xs) = lastM xs

--O(n2)
maximumM :: Ord a => [a] -> Maybe a --maximum pero total
maximumM [] = Nothing
maximumM xs = Just (maximum xs)

--O(n)
initM :: [a] -> Maybe [a] -- init pero total
initM [] = Nothing
initM xs = Just (init xs)
-- initM [x] = init [x]
-- initM (x:xs) = Just (x : initM xs)

--O(n)
get :: Int -> [a] -> Maybe a --Dado un indice devuelve un elem. en esa posicion
get _ [] = Nothing
get 0 (x:xs) = Just x
get n (x:xs) = get (n-1) xs

--O(n)
indiceDe :: Eq a => a -> [a] -> Maybe Int --Dado elem. y lista devuelve su posicion (al reves del anterior)
indiceDe e [] = Nothing
indiceDe e xs = Just (indiceDe' e xs)

indiceDe' :: Eq a => a -> [a] -> Int
indiceDe' e [] = Error "SE ROMPE TODO - NO PERTENECE ELEM AL INDICE"
indiceDe' e (x:xs) = if e==x 
    then 0
    else 1 + indiceDe' e xs



--O(n)
lookupM :: Eq k => [(k,v)] -> k -> Maybe v --Dado una lista de pares (key, value), le paso un key y me dice su value
lookUp [] _ = Nothing 
lookUp ((clave,valor):xs) key = if clave==key 
    then Just valor 
    else lookUp xs key  



--O(n)
fromJusts :: [Maybe a] -> [a] --Devuelve los valores de los maybe que no sean nothing
fromJusts (x:xs) = if x/=Nothing 
    then x : fromJusts xs 
    else fromJusts xs


--O(n?) 
minT :: Ord a => Tree a -> Maybe a --Devuelve elem. minimo del arbol
minT EmptyT = Nothing 
minT tree = Just (minT' tree) 


minT' :: Ord a => Tree a -> a
minT' (NodeT x EmptyT EmptyT) = x
minT' (NodeT x ti td) = min x (min (minT' td) (minT' ti))



