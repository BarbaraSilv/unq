import Practica1
--import Practica2

-- #################################################################################
-- #################################################################################
-- #################################################################################
-- #################################################################################
-- #################################################################################
-- ##########################  P R A C T I C A #####################################
-- ################################ T R E S ########################################
-- #################################################################################
-- #################################################################################
-- #################################################################################
-- #################################################################################

-- DUDAS GENERALES
-- 

--usando listas de listas = concatenar
--[x] ++ [x] = [x, x]
--x : [x] = [x, x]
--[x] : [x] = error 
--[x] : [[x]] = [[x], [x]]
--x : x : [] = [x,x]


-- RECURSION ESTRUCTURAL DE ARBOLES BINARIOS
-- f :: Tree a -> a
-- f EmptyT = ...
-- f (NodeT x ti td) = x ... f ti ... f td
-- ti = El nodo de la izquierda, td = el nodo de la derecha, x = El valor del nodo actual
-- EmptyT = Nodo nulo

--Posibles Errores =
    -- error de tipo
    -- error de pattern matching (falta considerar algun caso)
    -- infinite type = algun problema con el cons/falta algun pattern match para salir de la recursion
    -- error de sintaxis




data Tree a = EmptyT | NodeT a ( Tree a ) (Tree a ) deriving (Show)


arbol0 :: Tree Int
arbol0 = EmptyT

-- Al crear un arbol tengo que definir el Tipo
arbol1 :: Tree Int
arbol1 = NodeT 1
            (NodeT 2
                (NodeT 3
                    (NodeT 4 (EmptyT) (EmptyT)) 
                    (EmptyT))
                (EmptyT)) 
            (NodeT 5
                (NodeT 6 (EmptyT) (EmptyT))
                (NodeT 7 (EmptyT) (EmptyT)))
--
---           <-  1 -> 
---       <-  2      <- 5 ->
---    <- 3         6       7
---   4
---
---

arbol2 :: Tree Int
arbol2 = NodeT 1 (NodeT 2 (EmptyT) (EmptyT)) (NodeT 3 (EmptyT) (EmptyT))
-- T -> T
--    
--   -> T

arbol3 :: Tree [Int]
arbol3 = NodeT [1] (NodeT [2] (EmptyT) (EmptyT)) (NodeT [3] (EmptyT) (EmptyT))



arbolString :: Tree String
arbolString = NodeT "a" (NodeT "bb" (EmptyT) (EmptyT)) (NodeT "ccc" (NodeT "dddd" (EmptyT) (NodeT "eee" (EmptyT) (EmptyT))) (EmptyT)) 

sumarT :: Tree Int -> Int 
sumarT EmptyT = 0
sumarT (NodeT x ti td) = x + sumarT ti + sumarT td

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x ti td) = 1 + sizeT ti + sizeT td

mapDobleT :: Tree Int -> Tree Int 
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT x ti td) = NodeT (x*2) (mapDobleT ti) (mapDobleT td)

mapLongitudT :: Tree String -> Tree Int 
--Devuelve arbol con la longitud de cada palabra de un arbol de palabras
mapLongitudT EmptyT = EmptyT 
mapLongitudT (NodeT x ti td) = NodeT (longitud x) (mapLongitudT ti) (mapLongitudT td)

perteneceT :: Eq a => a -> Tree a -> Bool
--True si existe el elemento en el arbol
perteneceT _ EmptyT = False
perteneceT element (NodeT x ti td) = (x == element) || (perteneceT element ti) || (perteneceT element td)

aparicionesT :: Eq a => a -> Tree a -> Int
--Devuelve la cantidad de apariciones de a en el Arbol
aparicionesT _ EmptyT = 0
aparicionesT element (NodeT x ti td) = (devolverUnoSiSonIguales element x) + (aparicionesT element ti) + (aparicionesT element td)

devolverUnoSiSonIguales :: Eq a => a -> a -> Int
devolverUnoSiSonIguales element x = if element == x then 1 else 0

countLeaves :: Tree a -> Int 
--devolver cantidad de nodos sin hijos
countLeaves (NodeT _ (EmptyT) (EmptyT)) = 1
countLeaves (NodeT _ (ti) (EmptyT)) = countLeaves ti
countLeaves (NodeT _ (EmptyT) (td)) = countLeaves td
countLeaves (NodeT x ti td) = countLeaves ti + countLeaves td

leaves :: Tree a -> [a]
--devuelve todos los elementos que se encuentran en sus hojas
leaves EmptyT = []
leaves (NodeT x (EmptyT) (EmptyT)) = [x] 
--leaves (NodeT x (ti) (EmptyT)) = (leaves ti) -- No hace falta. Es Leave si de ambos lados tiene EmptyT, sino sigue buscando
--leaves (NodeT x (EmptyT) (td)) = (leaves td)
leaves (NodeT x ti td) =  (leaves ti) ++ (leaves td)

heightT :: Tree a -> Int 
----Devuelve altura del arbol (cantidad de niveles del arbol. Arbol vacio = 0, Arbol con una hoja = 1)
heightT EmptyT = 0 
heightT (NodeT _ EmptyT EmptyT) = 0
--heightT (NodeT _ ti EmptyT) = 1 + (heightT ti)
--heightT (NodeT _ EmptyT td) = 1 + (heightT td)
heightT (NodeT x ti td) = max (1 + (heightT ti)) (1 + (heightT td))

countNotLeaves :: Tree a -> Int 
--Devuelve el numero de nodos que no son hojas. Resolver sin usar recursion explicita. Primero definirla
--con recursion explicita y despues sin ella cant nodos - cant hojas
countNotLeaves (NodeT _ EmptyT EmptyT) = 0
countNotLeaves (NodeT _ ti EmptyT) = 1 + (countNotLeaves ti)
countNotLeaves (NodeT _ EmptyT td) = 1 + (countNotLeaves td)
countNotLeaves (NodeT x ti td) = 1 + (countNotLeaves ti) + (countNotLeaves td)

arbol8= NodeT 1 (EmptyT) ( NodeT 2 (NodeT 3 (EmptyT) (EmptyT)) (NodeT 4 (EmptyT) (EmptyT)))


mirrorT :: Tree a -> Tree a 
--Devuelve el arbol resultante de intercambiar el hijo izquierdo con el derecho, en cada nodo del arbol
mirrorT (NodeT x EmptyT EmptyT) = (NodeT x EmptyT EmptyT) 
mirrorT (NodeT x ti EmptyT) = NodeT x (EmptyT) (mirrorT ti)
mirrorT (NodeT x EmptyT td) = NodeT x (mirrorT td) (EmptyT)
mirrorT (NodeT x ti td) = NodeT x (mirrorT td) (mirrorT ti)


listInOrder :: Tree a -> [a]
-- primero los hijos del izquierdo, despues la raiz, despues los hijos del derecho
-- devuelve una lista con todos los elementos de todos los nodos del arbol
--deberia ser [4,3,2,1,6,5,7] 
listInOrder EmptyT = []
--listInOrder (NodeT x EmptyT EmptyT) = [x]
--listInOrder (NodeT x ti EmptyT) = [x] ++ (listInOrder ti) -- ++ De este lado para el caso (*)
--listInOrder (NodeT x EmptyT td) = [x] ++ (listInOrder td) 
listInOrder (NodeT x ti td) = (listInOrder ti) ++ [x] ++ (listInOrder td)


listPreOrder :: Tree a -> [a]
-- primero raiz, despues hijos del izquierdo, despues hijos del derecho
listPreOrder EmptyT = []
--listPreOrder (NodeT x EmptyT EmptyT) = [x]
--listPreOrder (NodeT x ti EmptyT) = x : (listPreOrder ti)
--listPreOrder (NodeT x EmptyT td) = x : (listPreOrder td)
listPreOrder (NodeT x ti td) = [x] ++ listPreOrder ti ++ listPreOrder td -- Daba lo mismo resolver combinando
--cons y ++ o haciendo todo con ++

listPosOrder :: Tree a -> [a]
-- primero hijos del izq, despues hijos del der, despues raiz
listPosOrder EmptyT = []
--listPosOrder (NodeT x EmptyT EmptyT) = [x]
--listPosOrder (NodeT x ti EmptyT) = [x] ++ (listPosOrder ti) 
--listPosOrder (NodeT x EmptyT td) = [x] ++ (listPosOrder td) 
listPosOrder (NodeT x ti td) = (listPosOrder ti)  ++ (listPosOrder td) ++ [x]


concatenarListasT :: Tree [a] -> [a]
-- dado un ARBOL DE LISTAS, NO UNA LISTA DE ARBOLES, concatena todos los elementos
concatenarListasT EmptyT = []
--concatenarListasT (NodeT x EmptyT EmptyT) = x
--concatenarListasT (NodeT x ti EmptyT) = x ++ concatenarListasT ti 
--concatenarListasT (NodeT x EmptyT td) = x ++ concatenarListasT td
concatenarListasT (NodeT x ti td) = x ++ (concatenarListasT ti) ++ (concatenarListasT td)

levelN :: Int -> Tree a -> [a]
--Devuelve una lista con todos los nodos del arbol que esten en el nivel N
levelN _ EmptyT = []
levelN 0 (NodeT x _ _) = [x]
levelN n (NodeT x ti td) = (levelN (n-1) ti) ++ (levelN (n-1) td)
--levelN 1 (NodeT x ti EmptyT) = returnNodeT ti
--levelN 1 (NodeT x EmptyT td) = returnNodeT td
 

--listPerLevel :: Tree a -> [[a]] 
--listPerLevel tree = listDesdeNivel 1 tree
--
--listDesdeNivel :: Int -> Tree a -> [[a]]
--listDesdeNivel n tree = if n <= (heightT tree)
--    then [levelN n tree] ++ listDesdeNivel (n+1) tree 
--    else []

listPerLevel :: Tree a -> [[a]]
----Devuelve lista de listas donde cada lista es un levelN del arbol
listPerLevel EmptyT = []
listPerLevel (NodeT x ti td) = [x] : juntarNiveles (listPerLevel ti) (listPerLevel td)

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles xss [] = xss
juntarNiveles [] yss = yss
juntarNiveles (xs:xss) (ys:yss) = (xs++ys) : juntarNiveles xss yss


{- NO FUNCA
ramaMasLarga :: Tree a -> [a]
--devuelve elementos de la rama mas larga del arbol
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x EmptyT EmptyT) = [x]
ramaMasLarga (NodeT x ti EmptyT) = x : ramaMasLarga ti
ramaMasLarga (NodeT x EmptyT td) = x : ramaMasLarga td
ramaMasLarga (NodeT x ti td) = x : ramaMasLarga (returnMaxT ti td)

returnMaxT :: Tree a -> Tree a -> Tree a 
returnMaxT tree1 tree2 = if heightT tree1 > (heightT tree2) then tree1 else tree2
-}

todosLosCaminos :: Tree a -> [[a]]
--Devuelve todos los caminos posibles desde la raiz hasta las hojas
--Ej arbol2 = 1, 2, 3 -->> [[1,2],[1,3]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x EmptyT EmptyT) = []
todosLosCaminos (NodeT x ti td) = agregarAtodas x (todosLosCaminos ti ++ todosLosCaminos td)

agregarAtodas :: a -> [[a]] -> [[a]]
agregarAtodas x [] = []
agregarAtodas x (xs:xss) = (x:xs) : agregarAtodas x xss

--balanceado :: Tree a -> Bool
----Un árbol está balanceado cuando para cada nodo la diferencia de alturas entre el subarbol izquierdo 
-- y el derecho es menor o igual a 1.



-- /////////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////////
-- /////////////////// MAPA DE TESOROS
-- /////////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////////


-- Si ya defini estos en la Practica2 me tira errores
data Dir = Izq | Der deriving (Show, Eq)
data Objeto = Tesoro | Chatarra deriving (Show, Eq)
data Mapa = Cofre Objeto | Bifurcacion Objeto Mapa Mapa deriving (Show)

mapa1 :: Mapa
mapa1 = (Bifurcacion Chatarra (Cofre Tesoro) (Cofre Chatarra))

mapa2 :: Mapa 
mapa2 = ( Bifurcacion Chatarra (Bifurcacion Chatarra (Cofre Tesoro) (Cofre Chatarra)) (Cofre Tesoro) )


hayTesoro :: Mapa -> Bool
--True si hay tesoro en todo el mapa
hayTesoro (Cofre obj) = obj == Tesoro
hayTesoro (Bifurcacion obj mapa1 mapa2) = obj == Tesoro || (hayTesoro mapa1) || (hayTesoro mapa2) 

hayTesoroEn :: [Dir] -> Mapa -> Bool 
-- True si hay tesoro al llegar al final de la lista de Direcciones
hayTesoroEn [] mapa = hayTesoro mapa
hayTesoroEn (d:dirs) (Bifurcacion obj mapa1 mapa2) = if d == Izq
    then hayTesoroEn (dirs) mapa1
    else hayTesoroEn (dirs) mapa2

--caminoAlTesoro :: Mapa -> [Dir]
---- Muestra el camino al unico tesoro que hay en el mapa (precondicion)
--caminoAlTesoro (Cofre Tesoro) = []
--caminoAlTesoro (Bifurcacion Tesoro _ _) = []
--caminoAlTesoro (Bifurcacion obj map1 map2) = if hayTesoro map1
--    then Izq : caminoAlTesoro map1
--    else Der : caminoAlTesoro map2

--caminoRamaMasLarga :: Mapa -> [Dir]
---- Indica el camino de la rama mas larga 
--caminoRamaMasLarga obj = []
----caminoRamaMasLarga (Bifurcacion x obj1 obj2) = []
--caminoRamaMasLarga (Bifurcacion x map1 obj) = Izq : caminoRamaMasLarga map1
--caminoRamaMasLarga (Bifurcacion x obj map2) = Der : caminoRamaMasLarga map2
--caminoRamaMasLarga (Bifurcacion x map1 map2) = if map1 == (returnMaxT map1 map2)
--    then Izq : caminoRamaMasLarga map1
--    else Der : caminoRamaMasLarga map2

-- returnMaxT :: Tree a -> Tree a -> Tree a 
-- returnMaxT tree1 tree2 = if heightT tree1 > (heightT tree2) then tree1 else tree2

--tesorosPerLevel :: Mapa -> [[Objeto]]
--Devuelve tesoros separados por nivel en el arbol

--todosLosCaminos :: Mapa -> [[Dir]]
-- Devuelve todos los caminos del mapa

-- //////////////////////////////////////////////////
-- //////////////////////////////////////////////////
-- //////////////////////////////////////////////////
-- MAPA DEL TESOROS:: LA REVANCHA
--Rehacer todas las anteriores teniendo en cuenta el nuevo tipo mapa 
--data Mapa = Cofre [Objeto]| Bifurcacion [Objeto] Mapa Mapa
-- //////////////////////////////////////////////////
-- //////////////////////////////////////////////////
-- //////////////////////////////////////////////////
















