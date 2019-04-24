-- //////////////////////////////////
-- //// C O S A S - G E N E R A L E S
-- ///////////////////////////////////

:: se puede resolver un ejercicio anidando ifs? SI, PERO OJO!!

:: Recursion implicita = dentro de otra funcion
:: Recursion explicita = dentro de la misma funcion

:: No puedo meter una func. recursiva entre [], porque sino pasa algo tipo: [[1], [[[[[[2]]]]]] ]

:: Equation gives different arities == FALTAN PARENTESIS O PARAMETROS O EL CONSTRUCTOR

:: Da lo mismo si el ejercicio se resuelve con ++ o con (:), mientras este bien

:: Se puede usar x:xs, c, ti, td, etc. Nombres poco descr. pero conocidos

:: Func. auxiliares salvo que sean muy complicadas no hace falta describir el proposito

:: Cuando termine el parcial comprobar parentesis y que no me haya olvidado de pasar algun parametro en ninguna funcion

:: Type error y uno de los parametros es un valor dentro de un Constructor = falta el Constructor

:: Este es un cons valido:   1 : 2 : 3 : 4 : 5 : []

:: Type a Does not match [a] = puede ser a causa de usar (:)

-- ///////////////////////////////
-- //// P R A C T I C A - U N O
-- ///////////////////////////////

sumatoria, promedio :: [Int] -> Int

primera, segunda, sumarPar, maxDelPar :: (Int, Int) -> Int

isEmpty :: [a] -> Bool

longitud :: [a] -> Int

mapSucesor :: [Int] -> [Int] --Devuelve lista de Int con el siguiente de cada Int

mapSumaPar :: [(Int,Int)] -> [Int] --Devuelve una lista con la suma de los elementos de cada par

mapMaxDelPar :: [(Int,Int)] -> [Int] --Devuelve una lista con el mayor de cada par

pertenece :: Eq a => a -> [a] -> Bool

apariciones :: Eq a => a -> [a] -> Int --Denota cuantas veces aparece a en la lista

filtrarMenoresA :: Int -> [Int] -> [Int] --Devuelve todos los elementos menores a n de la lista

filtrarElemento :: Eq a => a -> [a] -> [a] --Devuelve la lista sin apariciones de a

snoc :: [a] -> a -> [a] --retorna la lista con el elemento agregado al final de la lista

intercalar :: a -> [a] -> [a] --Ubica a entremedio de TODOS los ELEMENTOS de xs
-- Ej intercalar ',' "abcde" --> "a,b,c,d,e"

append :: [a] -> [a] -> [a] --dada dos listas devuelve una sola lista con todos los elem.

aplanar :: [[a]] -> [a] --dada una lista de listas, denota una lista con todos los elem

takeN, dropN :: Int -> [a] -> [a]

subtails :: [a] -> [[a]] --Ej [1,2,3] -> [[1,2,3],[1,2],[1],[]]

esPrefijo, esSufijo :: Eq a => [a] -> [a] -> Bool
--Prefijo Ej. [4,5] [1,2,3,4,5] --> True
-- Sufijo Ej. [1,2] [1,2,3,4,5] --> True

minimum, maximum :: Ord a => [a] -> a --Denota el minimo y maximo de toda la lista

reversa :: [a] -> [a] -- Da vuelta la lista

sacarMinimo :: Ord a => [a] -> [a] --Si el minimo aparece mas de una vez, sacar primer aparicion

diferencia :: Eq a => [a] -> [a] -> [a] -- primera lista - segunda lista = return

interseccion :: Eq a => [a] -> [a] -> [a] --Devuelve una lista que es la interseccion de dos listas
--Ej: [1,2,3,4] [3,4,5,6] --> [3,4]

ordenar :: Ord a => [a] -> [a] --devuelve lista ordenada de menor a mayor

desdeHasta :: Int -> Int -> [Int] --devuelve lista de numeros de n a m. Prec: n es menor a m


minimum' :: Ord a => [a] -> a 
minimum' [x] = x
minimum' (x:xs) = if x < minimum' xs then x else minimum' xs

maximum' :: Ord a => [a] -> a
maximum' [x] = x
maximum' (x:xs) = if x > maximum' xs then x else maximum' xs

snoc :: [a] -> a -> [a] --retorna la lista con el elemento agregado al final de la lista
snoc [] e = [e] 
snoc (l:ls) e = l:(snoc ls e)

intercalar :: a -> [a] -> [a] --Ubica a entremedio de TODOS los ELEMENTOS de xs
-- Ej intercalar ',' "abcde" --> "a,b,c,d,e"
intercalar _ [] = []   
intercalar e (x:xs) = x:e:( if (longitud xs) > 1 then intercalar e xs else xs )

append :: [a] -> [a] -> [a] --dada dos listas devuelve una sola lista con todos los elem
append [] ys = ys
append (x:xs) ys = x:(append xs ys)

takeN :: Int -> [a] -> [a]
takeN _ [] = []
takeN 0 xs = xs
takeN n (x:xs) = x : takeN (n-1) xs

dropN :: Int -> [a] -> [a]
dropN _ [] = []
dropN 0 xs = xs
dropN n (x:xs) = dropN (n-1) xs

subtails :: [a] -> [[a]]
--Ej [1,2,3] -> [[1,2,3],[1,2],[1],[]]
subtails [] = [[]]
subtails (x:xs) = (x:xs) : subtails xs

esPrefijo :: Eq a => [a] -> [a] -> Bool
--Devuelve True si la primera lista es prefijo de la segunda
--Ej [4,5] [1,2,3,4,5] --> True
esPrefijo [] [] = True
esPrefijo [] _ = True
esPrefijo _ [] = False
esPrefijo (x:xs) (y:ys) = x == y && esPrefijo xs ys

esSufijo :: Eq a => [a] -> [a] -> Bool 
--Ej [1,2] [1,2,3,4,5] --> True
esSufijo [] [] = True
esSufijo [] _ = True
esSufijo _ [] = False
esSufijo xs ys = last xs == (last ys) && esSufijo (init xs) (init ys)

sacarMinimo :: Ord a => [a] -> [a] --Si el minimo aparece mas de una vez, sacar primer aparicion
sacarMinimo [] = []
sacarMinimo [a] = [a]
sacarMinimo (x:xs) = if x == (minimum' (x:xs)) then xs else sacarMinimo xs

diferencia :: Eq a => [a] -> [a] -> [a]
-- primera lista - segunda lista = return
diferencia [] _ = []
diferencia (x:xs) ys = if pertenece x ys then diferencia xs ys else x : diferencia xs ys

interseccion :: Eq a => [a] -> [a] -> [a] --Devuelve una lista que es la interseccion de dos listas
--Ej: [1,2,3,4] [3,4,5,6] --> [3,4]
interseccion [] _ = []
interseccion [a] _ = [a]
interseccion (x:xs) ys = if pertenece x ys then x : interseccion xs ys else interseccion xs ys

ordenar :: Ord a => [a] -> [a]
--devuelve lista ordenada de menor a mayor
ordenar [] = []
ordenar ls = minimum' ls : ordenar (filtrarElemento (minimum' ls) ls)

desdeHasta :: Int -> Int -> [Int] --devuelve lista de numeros de n a m. Prec: n es menor a m
desdeHasta n m = if n == m  then m : [] else n : desdeHasta (n+1) m


-- ///////////////////////////////
-- //// P R A C T I C A - D O S
-- ///////////////////////////////

data Pizza = Prepizza | Agregar Ingrediente Pizza
data Ingrediente = Salsa | Queso | Jamon | AceitunasVerdes Int 

armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (i:ing) = Agregar i (armarPizza ing) 

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Agregar ing p) = if ing /= Jamon then Agregar ing (sacarJamon p) else sacarJamon p

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Agregar (AceitunasVerdes n) p) = Agregar (AceitunasVerdes (n*2)) (duplicarAceitunas p)
duplicarAceitunas (Agregar x p) = Agregar x (duplicarAceitunas p)


mayorNAceitunas :: Int -> [Pizza] -> [Pizza]
--Devuelve pizzas con mas de N aceitunas
--En el caso de Aceitunas que tiene un parametro (int), necesito si o si subtareas para fijarme, primero ingrediente
--por ingrediente y despues aceituna por aceituna
mayorNAceitunas 0 ls = ls
mayorNAceitunas _ [] = []
mayorNAceitunas n (p:pizzas) = if n < cantAceitunas p
        then p:(mayorNAceitunas n pizzas) 
        else mayorNAceitunas n pizzas


cantAceitunas :: Pizza -> Int
cantAceitunas Prepizza = 0
cantAceitunas (Agregar ing p) = cantAceitunas' ing + cantAceitunas p 


cantAceitunas' :: Ingrediente -> Int
cantAceitunas' (AceitunasVerdes n) = n
cantAceitunas' _ = 0


-- TESORO

data Objeto = Chatarra | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino


pasosHastaTesoro :: Camino -> Int -- Indica la cantidad de pasos hasta llegar al primer cofre con un tesoro
-- Precondicion: Hay Tesoro en el camino
--pasosHastaTesoro Fin = 0 No hace falta xq tiene precondicion de que si o si va a encontrar un Tesoro
pasosHastaTesoro (Nada cm) = 1 + pasosHastaTesoro cm 
pasosHastaTesoro (Cofre obj cm)= if hayTesoroEnLista obj then 0 else 1 + pasosHastaTesoro cm

hayTesoroEnLista :: [Objeto] -> Bool
hayTesoroEnLista [] = False
hayTesoroEnLista (x:xs) = x == Tesoro || hayTesoroEnLista xs 

cantidadDeTesorosEnCamino :: Camino -> Int
cantidadDeTesorosEnCamino Fin = 0
cantidadDeTesorosEnCamino (Nada c) = cantidadDeTesorosEnCamino c
cantidadDeTesorosEnCamino (Cofre lsObj Fin) = cantidadDeTesoros lsObj
cantidadDeTesorosEnCamino (Cofre lsObj c) = cantidadDeTesoros lsObj + cantidadDeTesorosEnCamino c



cantTesorosEntre :: Int -> Int -> Camino -> Int -- Dado un rango de pasos indica la cantidad de tesoros en ese rango
cantTesorosEntre n1 n2 camino = cantTesorosEntre' camino n1 (n2-n1)
-- Antes de empezar a sumar los tesoros, deberia posicionarme en el camino para que concuerde con el n1 que le paso, el "Desde"
-- Una vez que me posicione bien, solo es cuestion de ir sumando los tesoros Hasta n2
-- cantTesorosEntre' debe recibir n2 para poder pasarselo a la funcion sumarTesorosDelante
-- ¿¿¿POR QUE n2 - n1??? Suponer el siguiente camino:
-- [o0] - o1 - o2 - o3 - o4 - o5 - o6
-- Sabemos que sumarTesorosDelante suma el actual y los siguientes N pasos del camino que le pase como parametro
-- Supongamos que quiero saber los tesoros entre o3 y o5 (osea, desde 3 y hasta 5, dos parametros de la funcion principal)
-- Deberia posicionarme en o3 y ahi ejecutar sumarTesorosDelante
-- Si no hago (n2-n1) pasa lo siguiente:
-- Ejecuta cantTesorosEntre' 3 5 <camino>
-- X - X - X - [o3] - o4 - o5 - o6 
-- Una vez ahi, ejecuta sumarTesorosDelante <camino> 5
-- X - X - X - <!> - <!> - <!> - [<!>] 
-- Me pase!!! Conte de 3 a 6
-- Ahora, que pasa si hago (n2-n1), osea, 5-3 = 2?
-- cantTesorosEntre' 3 2
-- X - X - X - [o3] - o4 - o5 - o6 
-- Una vez ahi, ejecuta sumarTesorosDelante <camino> 2
-- X - X - X - <!> - <!> - [<!>] - o6 
-- Ahora si conto bien el rango entre 3 y 5

cantTesorosEntre' :: Camino -> Int -> Int -> Int 
-- Recibe un camino, un n1 "Desde" y un (n2-n1), se posiciona y ejecuta sumarTesorosDelante (que suma tambien el actual)
cantTesorosEntre' Fin _ _ = 0
cantTesorosEntre' (Cofre obj c) 0 n2 = sumarTesorosDelante (Cofre obj c) n2
cantTesorosEntre' (Nada c) 0 n2 = sumarTesorosDelante (Nada c) n2
cantTesorosEntre' (Nada c) n1 n2 = cantTesorosEntre' c (n1-1) n2
cantTesorosEntre' (Cofre obj c) n1 n2 = cantTesorosEntre' c (n1-1) n2

sumarTesorosDelante :: Camino -> Int -> Int
-- Dado un camino y un n cuenta la cantidad de cofres desde la posicion actual en el camino hasta n pasos adelante
sumarTesorosDelante Fin _ = 0
sumarTesorosDelante (Cofre obj c) 0 = sumarTesoros obj
sumarTesorosDelante (Nada c) 0 = 0
sumarTesorosDelante (Nada c) n = sumarTesorosDelante c (n-1)
sumarTesorosDelante (Cofre obj c) n = (sumarTesoros obj) + (sumarTesorosDelante c (n-1))




-- ///////////////////////////////
-- //// P R A C T I C A - T R E S
-- ///////////////////////////////

-- RECURSION ESTRUCTURAL DE ARBOLES BINARIOS
-- f :: Tree a -> a
-- f EmptyT = ...
-- f (NodeT x ti td) = x ... f ti ... f td

sumarT :: Tree Int -> Int 

sizeT :: Tree a -> Int

mapDobleT :: Tree Int -> Tree Int 

mapLongitudT :: Tree String -> Tree Int 

perteneceT :: Eq a => a -> Tree a -> Bool

aparicionesT :: Eq a => a -> Tree a -> Int

countLeaves :: Tree a -> Int 

countNotLeaves :: Tree a -> Int --Devuelve el numero de nodos que no son hojas

leaves :: Tree a -> [a]

heightT :: Tree a -> Int 

mirrorT :: Tree a -> Tree a 

listInOrder :: Tree a -> [a]

listPreOrder :: Tree a -> [a]

listPosOrder :: Tree a -> [a]

concatenarListasT :: Tree [a] -> [a]

levelN :: Int -> Tree a -> [a]

listPerLevel :: Tree a -> [[a]]

todosLosCaminos :: Tree a -> [[a]]


-- ///////////////////////////////////////////////////
-- ///////////////////////////////////////////////////


countLeaves :: Tree a -> Int --devolver cantidad de nodos sin hijos
countLeaves (NodeT _ (EmptyT) (EmptyT)) = 1
countLeaves (NodeT _ (ti) (EmptyT)) = countLeaves ti
countLeaves (NodeT _ (EmptyT) (td)) = countLeaves td
countLeaves (NodeT x ti td) = countLeaves ti + countLeaves td

countNotLeaves :: Tree a -> Int --Devuelve el numero de nodos que no son hojas
countNotLeaves (NodeT _ EmptyT EmptyT) = 0
countNotLeaves (NodeT _ ti EmptyT) = 1 + (countNotLeaves ti)
countNotLeaves (NodeT _ EmptyT td) = 1 + (countNotLeaves td)
countNotLeaves (NodeT x ti td) = 1 + (countNotLeaves ti) + (countNotLeaves td)


leaves :: Tree a -> [a] --devuelve todos los elementos que se encuentran en sus hojas
leaves EmptyT = []
leaves (NodeT x (EmptyT) (EmptyT)) = [x] 
leaves (NodeT x ti td) =  (leaves ti) ++ (leaves td)

heightT :: Tree a -> Int 
heightT EmptyT = 0 
heightT (NodeT _ EmptyT EmptyT) = 0
heightT (NodeT x ti td) = max (1 + (heightT ti)) (1 + (heightT td))

mirrorT :: Tree a -> Tree a 
--Devuelve el arbol resultante de intercambiar el hijo izquierdo con el derecho, en cada nodo del arbol
mirrorT (NodeT x EmptyT EmptyT) = (NodeT x EmptyT EmptyT) 
mirrorT (NodeT x ti EmptyT) = NodeT x (EmptyT) (mirrorT ti)
mirrorT (NodeT x EmptyT td) = NodeT x (mirrorT td) (EmptyT)
mirrorT (NodeT x ti td) = NodeT x (mirrorT td) (mirrorT ti)


listInOrder :: Tree a -> [a]
-- devuelve una lista con todos los elementos de todos los nodos del arbol
-- primero los hijos del izquierdo, despues la raiz, despues los hijos del derecho
listInOrder EmptyT = []
listInOrder (NodeT x ti td) = (listInOrder ti) ++ [x] ++ (listInOrder td)

listPreOrder :: Tree a -> [a]
-- primero raiz, despues hijos del izquierdo, despues hijos del derecho
listPreOrder EmptyT = []
listPreOrder (NodeT x ti td) = [x] ++ listPreOrder ti ++ listPreOrder td

listPosOrder :: Tree a -> [a]
-- primero hijos del izq, despues hijos del der, despues raiz
listPosOrder EmptyT = []
listPosOrder (NodeT x ti td) = (listPosOrder ti)  ++ (listPosOrder td) ++ [x]


concatenarListasT :: Tree [a] -> [a]
-- dado un ARBOL DE LISTAS (NO UNA LISTA DE ARBOLES), concatena todos los elementos
concatenarListasT EmptyT = []
concatenarListasT (NodeT x ti td) = x ++ (concatenarListasT ti) ++ (concatenarListasT td)

levelN :: Int -> Tree a -> [a]
--Devuelve una lista con todos los nodos del arbol que esten en el nivel N
levelN _ EmptyT = []
levelN 0 (NodeT x _ _) = [x]
levelN n (NodeT x ti td) = (levelN (n-1) ti) ++ (levelN (n-1) td)

listPerLevel :: Tree a -> [[a]]
----Devuelve lista de listas donde cada lista es un levelN del arbol
listPerLevel EmptyT = []
listPerLevel (NodeT x ti td) = [x] : juntarNiveles (listPerLevel ti) (listPerLevel td)

todosLosCaminos :: Tree a -> [[a]]
--Devuelve todos los caminos posibles desde la raiz hasta las hojas
--Ej arbol2 = 1, 2, 3 -->> [[1,2],[1,3]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x EmptyT EmptyT) = []
todosLosCaminos (NodeT x ti td) = agregarAtodas x (todosLosCaminos ti ++ todosLosCaminos td)

agregarAtodas :: a -> [[a]] -> [[a]]
agregarAtodas x [] = []
agregarAtodas x (xs:xss) = (x:xs) : agregarAtodas x xss

-- ///////////////////////////////
-- ////////////// NAVE
-- ///////////////////////////////

data Contenedor = Comida | Oxigeno | Torpedo | Combustible  deriving (Eq,Show)	
data Componente = Escudo | CanonLaser | Lanzatorpedos | Motor Int | Almacen [Contenedor] deriving (Eq,Show)	
data Nave = Parte Componente Nave Nave | ParteBase deriving Show


componentes :: Nave -> [Componente]
-- Retorna la lista de componentes.
componentes ParteBase = []
componentes (Parte c ti td) = [c] ++ componentes ti ++ componentes td 


poderDePropulsion :: Nave -> Int 
-- Retorna el poder de propulsion de una nave. El poder de propulsion de una nave es la suma de los poderes de 
--propulsion de los motores de la nave.
poderDePropulsion ParteBase = 0
poderDePropulsion (Parte c ti td) = sumarPropulsion c + (poderDePropulsion ti) + (poderDePropulsion td)

sumarPropulsion :: Componente -> Int 
sumarPropulsion (Motor n) = n
sumarPropulsion _ = 0


desarmarse :: Nave -> Nave 
--Reemplaza armas por escudos.
desarmarse ParteBase = ParteBase
desarmarse (Parte c ti td) = Parte (reemplazarArma c) (desarmarse ti) (desarmarse td)

reemplazarArma :: Componente -> Componente
reemplazarArma CannonLaser = Escudo
reemplazarArma Lanzatorpedos = Escudo
reemplazarArma c = c


cantidadComida :: Nave -> Int
--Dada una nave devuelve la cantidad de comida. Cada aparici ́on de Comida vale 1.
cantidadComida ParteBase = 0
cantidadComida (Parte c ti td) = contarComida c + (cantidadComida ti) + (cantidadComida td)

contarComida :: Componente -> Int
contarComida (Almacen xs) = sumarComida xs 
contarComida _ = 0

sumarComida :: [Contenedor] -> Int 
sumarComida [] = 0
sumarComida (x:xs) = if x == Comida 
    then 1 + sumarComida xs 
    else sumarComida xs


naveToTree :: Nave -> Tree Componente 
--Dada una nave la transforma en un  ́arbol de componentes
naveToTree ParteBase = EmptyT
naveToTree (Parte c ti td) = NodeT c (naveToTree ti) (naveToTree td)


aprovisionados :: [Contenedor] -> Nave -> Bool 
--Dada una lista de contenedor chequea que cada almacen contenga todos esos tipos de contenedores.
--Prec: Hay al menos un almacen en la nave
--Chequea que el almacen tiene al menos 1 aparicion
aprovisionados lsCont ParteBase = True 
aprovisionados lsCont (Parte c ti td) = (almacenTieneTodo lsCont c) && (aprovisionados lsCont ti) && (aprovisionados lsCont td)

almacenTieneTodo :: [Contenedor] -> Componente -> Bool
almacenTieneTodo [] c = True 
almacenTieneTodo (x:xs) (Almacen lsCont) = (almacenTiene x lsCont) && (almacenTieneTodo xs (Almacen lsCont))
-- <!!> OJO!!!! ME HABIA OLVIDADO DE PONER ALMACEN Y PUSE LSCONT.
almacenTieneTodo ls componente = True 

almacenTiene :: Contenedor -> [Contenedor] -> Bool 
almacenTiene x lsCont = elem x lsCont


armasNivelN :: Int -> Nave -> [Componente]
armasNivelN 0 ParteBase = []
armasNivelN 0 (Parte x _ _) = concatenarArma x
armasNivelN n (Parte _ ti td) = (armasNivelN (n-1) ti) ++ (armasNivelN (n-1) td )

concatenarArma :: Componente -> [Componente]
concatenarArma CanonLaser = [CanonLaser]
concatenarArma Lanzatorpedos = [Lanzatorpedos]
concatenarArma _ = []



-- ///////////////////////////////////////////
-- ////////////// ORGANIZADOR
-- ///////////////////////////////////////////

type Software = String
data Autor = Admin String | Dev String deriving (Show, Eq)
data Organizador = Agregar Software [Autor] Organizador | Vacio deriving (Show)


filtrar :: [Autor] -> Organizador -> Organizador
--dado un conjunto de autores y un organizador, elimina esos autores de cada software.
filtrar lsAutor Vacio = Vacio 
filtrar xs (Agregar soft lsAutor org) = (Agregar soft (diferencia lsAutor xs) (filtrar xs org))




losAdmin :: Organizador -> [Autor]
--denota una lista con todos los administradores, sin elementos repetidos.
losAdmin Vacio = []
losAdmin (Agregar soft lsAutor org) = (admins lsAutor) ++ (losAdmin org)
-- OJO! Este no se podia resolver usando (:) en lugar de ++, error de tipo al hacer [] : []

admins :: [Autor] -> [Autor]
admins [] = []
admins (a:autores) = if esAdmin a 
    then a : admins autores 
    else admins autores 

esAdmin :: Autor -> Bool
esAdmin (Admin name) = True 
esAdmin _ = False




ordenados :: Organizador -> [Software]
--dado un organizador, denota la lista de programas ordenados de menor a mayor por cantidad de autores.
ordenados Vacio = []
ordenados org = [softConMenorAutores org] 
                        ++ ordenados (filtrarSoftware (softConMenorAutores org) org) 
-- filtrarElemento no me sirve para resolver este caso, xq recibe a y [a], no a y <tipo algebraico>

filtrarSoftware :: Software -> Organizador -> Organizador
filtrarSoftware soft Vacio = Vacio
filtrarSoftware soft (Agregar s a org) = if soft == s 
    then filtrarSoftware soft org -- Podria haber sido parcial y en lugar de esto devolver directo el org
    else (Agregar s a (filtrarSoftware soft org)) 

softConMenorAutores :: Organizador -> Software
softConMenorAutores (Agregar soft lsAutor Vacio) = soft
softConMenorAutores (Agregar soft lsAutor org) = if (longitud lsAutor) < (longitud (softConMenorAutores' org))
    then soft 
    else softConMenorAutores org

softConMenorAutores' :: Organizador -> [Autor]
softConMenorAutores' (Agregar soft lsAutor Vacio) = lsAutor
softConMenorAutores' (Agregar soft lsAutor org) = if (longitud lsAutor) < (longitud (softConMenorAutores' org)) 
    then lsAutor
    else softConMenorAutores' org
