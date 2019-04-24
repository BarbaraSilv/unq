module Practica1 where

-- /////////////////////////////////////////////////
-- /////////////////////////////////////////////////
-- ///////////// 1 - BASICO
-- /////////////////////////////////////////////////
-- /////////////////////////////////////////////////

-- lsNum, lsNum2, lsParNum
-- lsString, lsChar, lsBool, lsNull
-- 

--recorrido [] = ...
--recorrido (x:xs) = ... recorrido xs //// (x:xs) == Recursion Estructural

--maximo :: [Int] -> [Int]
--maximo (x:[]) = x
--maximo (x:xs) = max x (maximo xs)

lsNull :: [a]
lsNull = []

lsNum :: [Int]
lsNum = [1,2,3,4,5,6,7,8,9]

lsNum2 :: [Int]
lsNum2 = [1,1,1,2,2,2,3,3,3]

lsNum3 :: [Int]
lsNum3 = [-3,-2,-1,0,1,2,3]

lsString :: String
lsString = "a b c d e"

lsChar :: [Char]
lsChar = ['a','b','c','d','e']

lsBool :: [Bool]
lsBool = [True,True,False,False]

lsParNum :: [(Int,Int)]
lsParNum = [(1,1),(2,2),(3,3)]


-- /////////////////////

dobleL :: [Int] -> [Int]
dobleL [] = []
dobleL (x:xs) = x * 2 : dobleL xs

sucesor :: Int -> Int
sucesor = succ --aplicacion parcial

sumar :: Int -> Int -> Int 
sumar n2 = (+ n2) --aplicacion parcial

maximo :: Int -> Int -> Int
maximo = max --app parcial

negar :: Bool -> Bool
negar = not

--Lo comento porque cuando pongo a me autocompleta y es re molesto wee
--andLogico :: Bool -> Bool -> Bool
--andLogico v2 = (&& v2)


orLogico :: Bool -> Bool -> Bool
orLogico v2 = (|| v2) 

primera :: (Int,Int) -> Int
primera (x,_) = x

segunda :: (Int,Int) -> Int
segunda (_,y) = y

sumarPar :: (Int,Int) -> Int
sumarPar (x,y) = sumar x y

maxDelPar :: (Int,Int) -> Int
maxDelPar (x,y) = maximo x y

loMismo :: a -> a 
loMismo x = x

siempreSiete :: a -> Int
siempreSiete _ = 7

duplicar :: a -> (a,a)
duplicar a = (a,a)

singleton :: a -> [a]
singleton a = [a]


isEmpty :: [a] -> Bool
--isEmpty [] = null []
--isEmpty [a] = null [a]

isEmpty = null


head' :: [a] -> a 
head' = head 

tail' :: [a] -> [a] 
tail' = tail 

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs --NO VA MASSSS

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum xs

longitud :: [a] -> Int
--longitud [] = 0
--longitud ls = 1 + longitud (tail ls) -- NO VA MASSSSS
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


-- /////////////////////////////////////////////////
-- /////////////////////////////////////////////////
-- ///////////// 2.1 - RECURSION
-- /////////////////////////////////////////////////
-- /////////////////////////////////////////////////



mapSucesor :: [Int] -> [Int]
--mapSucesor ls = map succ ls -- Implementacion rapida, pero creo que con recursion es de otra forma
mapSucesor [] = []
mapSucesor (x:xs) = [x+1] ++ mapSucesor xs

mapSumaPar :: [(Int,Int)] -> [Int] --Devuelve una lista con la suma de los elem de cada par
--ls = [(1,2),(3,4)] -> [3,7]
mapSumaPar [] = []
mapSumaPar (x:xs) = [sumarPar x] ++ mapSumaPar xs

mapMaxDelPar :: [(Int,Int)] -> [Int]
--ls = [(1,2),(3,4)] -> [2,4]
mapMaxDelPar [] = []
-- Implementacion anterior segun Mumuki:
--mapMaxDelPar ls = [maxDelPar (head ls)] ++ mapMaxDelPar (tail ls)
mapMaxDelPar (x:xs) = [maxDelPar x] ++ mapMaxDelPar xs

todoVerdad :: [Bool] -> Bool --Denota True si todos los elem son True
todoVerdad [] = True 
todoVerdad (x:xs) = (True && x) && todoVerdad xs

algunaVerdad :: [Bool] -> Bool
algunaVerdad [] = False
algunaVerdad (x:xs) = x || algunaVerdad xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = (e == x) || (pertenece e xs)

lsApariciones = [1,2,3,4,5] --SUPER TRUCO PARA PROMOCIONAR CORTESIA DE GONZALO
apariciones :: Eq a => a -> [a] -> Int
--Denota cuantas veces aparece e en xs
apariciones _ [] = 0
apariciones e (x:xs) =  (apariciones' e x) + (apariciones e xs) --deconstruccion de datos + pattrn matching
-- //
apariciones' :: Eq a => a -> a -> Int
apariciones' e1 e2 = if e1 == e2  then 1 else 0 --if es la forma mas correcta? RTA: es la unica forma
--(sumarAparicion' e xs) + (apariciones e (tail xs))


filtrarMenoresA :: Int -> [Int] -> [Int] --Devuelve todos los elementos menores a n de la lista xs
filtrarMenoresA _ [] = []
filtrarMenoresA n (x:xs) = if x > n then x:filtrarMenoresA n xs else filtrarMenoresA n xs


filtrarElemento :: Eq a => a -> [a] -> [a] --Devuelve la lista sin apariciones de e
filtrarElemento _ [] = []
filtrarElemento e (x:xs) = if e == x then filtrarElemento e xs else x:filtrarElemento e xs

mapLongitudes :: [[a]] -> [Int] --denota lista con longitudes
mapLongitudes [] = []
mapLongitudes (x:xs) = [longitud x] ++ mapLongitudes xs

longitudMayorA :: Int -> [[a]] -> [[a]] -- Dados un número n y una lista de listas,
-- devuelve la lista de aquellas listas que tienen más de n elementos.
-- Ej. 3 [[1,2,3,4],[1,2],[1,2,3,4,5]] --> [[1,2,3,4],[1,2,3,4,5]]
longitudMayorA _ [] = []
longitudMayorA  n (x:xs) = 
    if longitud x > n 
    then x:longitudMayorA n xs 
    else longitudMayorA n xs
--siempre el caso x:funcion xs ponerlo en el THEN, no en el ELSE

--longitudMayorA n (x:xs) = longitudMayorA' n x :( longitudMayorA n xs ) 
--longitudMayorA' :: Int -> [a] -> [a]    
--longitudMayorA' n ls = if longitud ls > n then ls else [] --Lo pude hacer con deconstruccion/recurs. estructural!! obsoleto

intercalar :: a -> [a] -> [a] --Ubica a entremedio de TODOS los ELEMENTOS de xs
-- Ej intercalar ',' "abcde" --> "a,b,c,d,e"
intercalar _ [] = []   
intercalar e (x:xs) = x:e:( if (longitud xs) > 1 then intercalar e xs else xs ) --le faltaba un parametro a intercalar
--con not null tampoco porque me queda "a,b," y error de tipo

snoc :: [a] -> a -> [a]
--retorna la lista con el elemento agregado al final de la lista
snoc [] e = [e] 
snoc (l:ls) e = l:(snoc ls e)

append :: [a] -> [a] -> [a]
--dada dos listas devuelve una sola lista con todos los elem
append [] ys = ys
append (x:xs) ys = x:(append xs ys)

aplanar :: [[a]] -> [a]
--dada una lista de listas, denota una lista con todos los elem
aplanar [] = [] 
aplanar (x:xs) = append x (aplanar xs)

reversa :: [a] -> [a]
--da vuelta la lista
reversa [] = []
reversa (x:xs) = snoc (reversa xs) x  

zipMaximos :: [Int] -> [Int] -> [Int]
--Dadas dos listas de enteros, devuelve una lista donde el elemento en la posicion N es el máximo entre el 
--elemento N de la primera lista y de la segunda lista, teniendo en cuenta que las listas no necesariamente tienen 
--la misma longitud.
--Ej: [2,0,0] [1,3,3] --> [2,3,3], [2,0,0] [3] --> [3,0,0]
zipMaximos [] [] = []
zipMaximos a [] = a
zipMaximos [] a = a
zipMaximos (x:xs) (y:ys) = (max x y) : (zipMaximos xs ys)


zipSort :: [Int] -> [Int] -> [(Int, Int)]
--Dadas dos listas de enteros devuelve una lista de pares con el min y max de cada posicion
--Ej: [1,2,3,4] [0,0,5,5] --> (0,1)(0,2)(3,5)(4,5)
zipSort [] [] = []
--zipSort a [] = a
--zipSort [] a = a
zipSort (x:xs) (y:ys) =  ( (min x y) , (max x y) )  : zipSort xs ys

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--SI USO ":", LO DE LA IZQUIERDA NO ES UNA []!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

promedio :: [Int] -> Int   
promedio xs = sumatoria xs `div` longitud xs --ESTO FUNCIONA PERO NO ES RECURSION ESTRUCTURAL PERO EL EJERCICIO NO SE PUEDE HACER CON RECURSION!!!

minimum' :: Ord a => [a] -> a 
--Me devuelve el minimo de toda la lista
minimum' [x] = x
minimum' (x:xs) = if x < minimum' xs then x else minimum' xs

-- /////////////////////////////////////////////////
-- /////////////////////////////////////////////////
-- //////////// 2.2 RECURSION SOBRE NUMEROS
-- /////////////////////////////////////////////////
-- /////////////////////////////////////////////////


factorial :: Int -> Int
-- 5 = 5*4*3*2*1
factorial 0 = 1
factorial n = factorial (n-1) * n

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = [n] ++ cuentaRegresiva (n-1)

contarHasta :: Int -> [Int]
contarHasta 0 = []
contarHasta n = contarHasta(n-1) ++ [n]

replicarN :: Int -> a -> [a] --devuelve una lista de "e" con n elementos
replicarN 0 _ = []
replicarN n e = e : replicarN (n-1) e 

--replicarN :: Int -> a -> [a]
--replicarN 0 e = []
--replicarN n e = if n <= 0 then replicarN n e
--                        else e : replicarN (n-1) e

--desdeHasta :: Int -> Int -> [Int] --devuelve lista de numeros de n a m
--desdeHasta (n - m) (m - n) = []
--SI PONGO LOS NUMEROS AL REVES SE ROMPE TODO!! OJO!!
--desdeHasta n m = if n-1 /= m then n : desdeHasta (n+1) m else []


desdeHasta :: Int -> Int -> [Int] --devuelve lista de numeros de n a m
desdeHasta n m = if n == m 
        then m : []
        else n : desdeHasta (n+1) m


takeN :: Int -> [a] -> [a]
takeN _ [] = []
--takeN n (x:xs) = if n > 0 then x:takeN (n-1) xs else [] Otra forma para resolverlo, con un pattern match. más
takeN 0 xs = xs
takeN n (x:xs) = x : takeN (n-1)  xs

dropN :: Int -> [a] -> [a]
dropN _ [] = []
dropN 0 xs = xs
dropN n (x:xs) = dropN (n-1) xs

splitN :: Int -> [a] -> ([a],[a])
--Devuelve una par con una lista que resulta de aplicar dropN y otra lista que resulta takeN
splitN n ls = (takeN n ls, dropN n ls)

-- /////////////////////////////////////////////////
-- /////////////////////////////////////////////////
-- ///////////////////    ANEXO   //////////////////
-- /////////////////////////////////////////////////
-- /////////////////////////////////////////////////

maximum' :: Ord a => [a] -> a
-- Me devuelve el minimo de la lista
maximum' [x] = x
maximum' (x:xs) = if x > maximum' xs then x else maximum' xs

splitMin :: Ord a => [a] -> (a, [a])
--devuelve el minimo y la lista sin el minimo
splitMin xs = (minimum' xs, filtrarElemento (minimum' xs) xs)


ordenar :: Ord a => [a] -> [a]
--devuelve lista ordenada de menor a mayor
--ordenar [a] = [a]
ordenar [] = []
--ordenar (x:xs) = minimum' (x:xs) : etc //No funciona porque no vamos a recursionar e ir sacando el head, sino que vamos
-- sacando el minimum
ordenar ls = minimum' ls : ordenar (filtrarElemento (minimum' ls) ls)

interseccion :: Eq a => [a] -> [a] -> [a]
--Devuelve una lista que es la interseccion de dos listas
--Ej: [1,2,3,4] [3,4,5,6] --> [3,4]
interseccion [] _ = []
interseccion [a] _ = [a]
interseccion (x:xs) ys = if pertenece x ys then x : interseccion xs ys else interseccion xs ys

--[1,2,3,4] [3,4,5,6]
--[2,3,4] [3,4,5,6]
--[3,4] [3,4,5,6]
--[3 ++ [4] [3,4,5,6]]
--[[3,4] ++ [] [3,4,5,6]]

diferencia :: Eq a => [a] -> [a] -> [a]
-- primera lista - segunda lista = return
diferencia [] _ = []
diferencia (x:xs) ys = if pertenece x ys then diferencia xs ys else x : diferencia xs ys

particionPorParidad :: [Int] -> ([Int], [Int])
--una tupla con dos listas, una con numeros pares, otra con impares
particionPorParidad xs = (particionDePares xs,particionDeImpares xs)

particionDePares :: [Int] -> [Int]
particionDePares [] = []
particionDePares (x:xs) = if (x `mod` 2) == 0 then x:particionDePares xs else particionDePares xs

particionDeImpares :: [Int] -> [Int]
particionDeImpares [] = []
particionDeImpares (x:xs) = if (x `mod` 2 == 1) then x:particionDeImpares xs else particionDeImpares xs

particionPorSigno :: [Int] -> ([Int], [Int])
particionPorSigno xs = (particionDePositivos xs,particionDeNegativos xs)

particionDePositivos :: [Int] -> [Int]
particionDePositivos [] = []
particionDePositivos (x:xs) = if x >= 0 then x:particionDePositivos xs else particionDePositivos xs

particionDeNegativos :: [Int] -> [Int]
particionDeNegativos [] = []
particionDeNegativos (x:xs) = if x < 0 then x:particionDeNegativos xs else particionDeNegativos xs


subtails :: [a] -> [[a]]
--Ej [1,2,3] -> [[1,2,3],[1,2],[1],[]]
subtails [] = [[]]
subtails (x:xs) = (x:xs) : subtails xs

--agrupar :: Eq a => [a] -> [[a]] -- <??????????> <??>
----Ej [1,1,2,3,3,3] --> [[1,1],[2],[3,3,3]]
--agrupar [] = [[]]
--agrupar [a] = [[a]]
----agrupar (x:plagio) = if x == head plagio 
----                then ((head (agrupar plagio)) ++ [x]) : (tail (agrupar plagio))
----                else [x] : (agrupar plagio)
--
--agrupar (x:y:ls) = if x == y
--    then [x] ++ [y] : agrupar ls
--    else [x] : agrupar ls


--No puedo meter una func. recursiva entre [], porque sino pasa algo tipo: [[1], [[[[[[2]]]]]] ]


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


sacarMinimo :: Ord a => [a] -> [a]
--Si el minimo aparece mas de una vez, sacar primer aparicion, no sacar todas las apariciones
--Osea no se puede resolver usando filtrarElemento
sacarMinimo [] = []
sacarMinimo [a] = [a]
sacarMinimo (x:xs) = if x == (minimum' (x:xs)) then xs else sacarMinimo xs



