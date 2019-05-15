import MapSinRep -- emptyM, assocM (agregar), lookupM (buscar TOTAL, devuelve Just value/nothing), deleteM (borrar PARCIAL), domM ([keys])
import MapConRep 
import MapConList -- <??> puede tener claves repetidas o no?
import MapOrdenado -- <??> puede tener claves repetidas o no?

import Celda -- WIP <??>

import Multiset -- WIP <??>
import MultisetMap -- WIP <??>

import BST -- <??> Todas las funciones se hacen como si fuera el implementador? No hay ninguna como si fuera el usuario?
import MapBST -- WIP
import SetBST -- WIP

import Heap -- WIP <??> heapSort la implemento como si fuera un usuario o como si fuera el implementador?
import HeapBST -- WIP <??> Invariantes de binaryHeap? (ver practica)



data Maybe a = Nothing | Just a deriving Show
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show


-- /////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////
-- /////////////////////////////////////  M A P
-- /////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////

--O(n2)
buscarClaves :: Eq k => [k] -> Map k v -> [Maybe v]
buscarClaves [] _ = Nothing 
buscarClaves (key:lsKey) map = if elem key (domM map) 
    then Just key : (buscarClaves lsKey map) -- PUEDO HACER JUST <recursion> XQ AL FINAL ME DA [Maybe v]
    else buscarClaves lsKey map 



--O(n2)
estanTodas :: Eq k => [k] -> Map k v -> Bool 
estanTodas [] _ = True 
estanTodas (key:lsKey) map = elem key (domM map) && (estanTodas lsKey map)

--O
actualizarClaves :: Eq k => [(k, v)] -> Map k v -> Map k v 
--Para cada clave, le cambio el valor que tiene en el Map (o no). Vale decir que
--no necesariamente le paso todas las claves que existen en el Map.
actualizarClaves [] map = map 
actualizarClaves ((key, value):xs) = 
-- En el map qué pasa si inserto un (k,v) que en el map, el k ya existe? Pueden existir dos valores asociados
-- a una misma clave? si inserto un key con un value nuevo, sobreescribe al value anterior? <??>

--O(n2)
unirDoms :: Eq k => [Map k v] -> [k]
unirDoms lsmap = sinRepetidos (unirDoms' lsmap)

--O(n2)
unirDoms' :: Eq k => [Map k v] -> [k] 
unirDoms' [] = []
unirDoms' (map:lsMap) = domM map : (unirDoms lsMap)

-- <??> No se que eficiencia tiene esto
mapSuccM :: Eq k => [k] -> Map k Int -> Map k Int 
--Le suma 1 a c/num asociado con las claves de la lista
mapSuccM [] map = map 
mapSuccM (k:lsKey) map = 
    if lookupM map k == Nothing 
    then mapSuccM lsKey map 
    else assocM ( deleteM (mapSuccM lsKey map) k) k (lookupM map k)+1 -- <??> lookupM me convierte el v
    -- en Just v. Como revierto esto para poder resolver este? 

agregarMap :: Eq k => Map k v -> Map k v -> Map k v 
-- agregar claves y valores del primer map en el segundo. Si ya existe la clave, la reemplaza
agregarMap map1 map2 = agregarMap' (domM map1) map2 

-- <??> Puedo extender la interfaz del Map para resolver este? implementar un imgM o algo que me de la lista
--de valores del Map. 
--Si no se puede, hay que tomar map1, lookup el k (que no se encuentra en map2) para tomar el valor
--que va dentro del assocM del then para poder ingresarlo (antes convertir Just v en v).
--O(n2) ¿amortizado? --<??> a veces va a hacer assoc delete agregarMap, haciendo que empeore la eficiencia, 
--pero no siempre
agregarMap' :: Eq k => [k] -> Map k v -> Map k v 
agregarMap' [] _ = emptyM
agregarMap' (k:lsKey) map2 = if lookupM map2 k == Nothing 
    then assocM (agregarMap' lsKey map2) k ¿v? 
    else assocM ( deleteM (agregarMap' lsKey map2) k) k ¿v? 

indexar :: [a] -> Map Int a 
--construye un map con la lista de valores
indexar [] = emptyM
indexar (v:valores) = assocM (indexar valores) v n --ESTO SE RESUELVE CON LET, USANDO ALGUNA "VARIABLE" 
--QUE CON CADA ITERACION SUME 1 A N. 

ocurrencias :: String -> Map Char Int
--Cuenta cuanto aparece cada letra del string, usando un Map 
--Ej. oso ---> (o,2) (s,1) (o,2), sansa ---> (s,2) (a,2) (n,1) (s,2) (a,2) <??> Asi?
ocurrencias [] = emptyM
ocurrencias s:string = assocM (ocurrencias string) s n --ESTO SE RESUELVE CON LET, USANDO ALGUNA "VARIABLE" 
--QUE CON CADA ITERACION HAGA LOOKUP DEL STRING ENTERO Y SE FIJE CUANTAS VECES APARECE


-- /////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////
-- ///////////////////////////////////// C E L D A
-- /////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////

-- celdaVacia, poner (color celda), sacar (color celda), hayBolitas (color celda), nroBolitas (color celda)

nroBolitasMayorA :: Color -> Int -> Celda -> Bool 
--True si en la celda hay más bolitas de color que n
nroBolitasMayorA color n celda = (nroBolitas color celda) > n


ponerN :: Int -> Color -> Celda -> Celda 
ponerN 0 _ celda = celda
ponerN n color celda = ponerN (n-1) color (poner color celda)


hayBolitasDeCadaColor :: Celda -> Bool 
hayBolitasDeCadaColor celda = 
    (hayBolitas Rojo celda) && (hayBolitas Azul celda) && (hayBolitas Negro celda) && (hayBolitas Verde celda)
-- <??> Esto se puede hacer??

-- /////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////
-- ///////////////////////////////////// M U L T I S E T
-- /////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////

ocurrencias :: String -> Map Char Int
--Cuenta cuanto aparece cada letra del string, usando un Map 
--Ej. oso ---> (o,2) (s,1) (o,2), sansa ---> (s,2) (a,2) (n,1) (s,2) (a,2) <??> Asi?
