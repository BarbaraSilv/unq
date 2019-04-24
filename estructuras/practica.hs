doble :: Int -> Int
doble x = x + x

mul:: Int -> Int-> Int-> Int
mul a b c = a * b * c

f:: Int-> Int
f x= x + 1

g:: Int -> Int -> Int
g x y = x * y + y

h:: Bool -> Int -> Int -> Bool
h b x y = b && (x < y)

--segundosEnTotal:: Int -> Int -> Int -> Int -> Int 
--segundosEnTotal d h m s = segundosEnDias  d 
--						+ segundosEnHoras h
--						+segundosEnMinutos m
--						+s

--segundosEnDias:: Int -> Int
--segundosEnDias d = segundosEnHoras 24 * d

--segundosEnHoras :: Int -> Int 


minimo:: Int -> Int -> Int
minimo a b = if a > b then b else a 

minimo3::Int -> Int -> Int -> Int
minimo3  a b c = minimo (minimo a b) c 

--and' :: Bool -> Bool ->Bool
--and' p q = if p then q else p 

--and'' True True = True
--and'' True False = False
--and'' False True = False
--and'' False False = False

--and'' True q = q
--and'' False _  = False

--and'' True True = True
--and'' _ _ = False


--Practica 1
----1
--a
sucesor :: Int -> Int
sucesor a = a + 1

--b
sumar :: Int -> Int -> Int
sumar a b = a + b

--c	
maximo :: Int -> Int -> Int
maximo a b  = if a > b 
				then a 
				else b 

----2 
--a
negar :: Bool -> Bool

negar a = not a

--b
andLogico :: Bool  -> Bool -> Bool
andLogico  p q = if p then q else p

--d
primera :: (Int,Int) -> Int

--Dado un par de números devuelve la primera componente.

primera (a,b) = fst (a,b)

--e
segunda :: (Int,Int) -> Int
segunda (a,b) = snd (a,b)

--f
sumaPar :: (Int,Int) -> Int
--Dado un par de números devuelve su suma.
sumaPar (a,b) = sumar (primera(a,b)) (segunda(a,b))

--g
maxDelPar :: (Int,Int) -> Int
--Dado un par de números devuelve el mayor de estos.
maxDelPar (a,b) = maximo (primera(a,b)) (segunda(a,b))

----3
--a
loMismo :: a -> a
--Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo a = a 

siempreSiete :: a -> Int
--Dado un elemento de algún tipo devuelve el número 7. 
siempreSiete a = 7

--c 
duplicar :: a -> (a,a)

--Dado un elemento de algún tipo devuelve un par con ese elemento en ambas compo-
--nentes.


duplicar a = (a,a)

--d 
singleton :: a -> [a]
--Dado un elemento de algún tipo devuelve una lista con este único elemento.
singleton a = [a]


----4
lista1 = [1,8,4,1]
--a
isEmpty :: [a] -> Bool

isEmpty [] =null []
isEmpty [a] = null [a]
--b

head' :: [a] -> a
head' (x:s) = x
--c

tail' :: [a] -> [a]
tail' (x:xs) = xs

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
----------2 Recursión 
---1 
sumatoria :: [Int] -> Int
--Dada una lista de enteros devuelve la suma de todos los elementos 
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--2
longitud :: [a] -> Int
--Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
--de elementos que posee.
longitud [] = 0
--longitud ls = 1 + longitud (tail ls)
longitud (a:xs) = 1 + longitud xs
--3

ls = [1,2,3,4,5]
mapSucesor :: [Int] -> [Int]
--Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
mapSucesor [] = []
mapSucesor (x:xs) = (x + 1) : mapSucesor xs
  
--4
mapSumaPar :: [(Int,Int)] -> [Int]
{--
Dada una lista de pares de enteros, devuelve una nueva lista en la que cada elemento es la
suma de los elementos de cada par.
--}
mapSumaPar [] = []
mapSumaPar (x:xs) = sumaPar x : mapSumaPar xs

--5
mapMaxDelPar :: [(Int,Int)] -> [Int]
{--
Dada una lista de pares, devuelve una nueva lista en la que cada elemento es el mayor de
las componentes de cada par.
--}
mapMaxDelPar [] = []
mapMaxDelPar (x:xs) = maxDelPar x : mapMaxDelPar xs

--6

todoVerdad :: [Bool] -> Bool
--Dada una lista de booleanos devuelve True si todos sus elementos son True.
todoVerdad [] = True
todoVerdad (x:xs)= (x == True) && todoVerdad xs  

--------------------------------------------------------------------------------------

algunaVerdad :: [Bool] -> Bool
--Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
algunaVerdad [] = False
algunaVerdad (x:xs) = (x==True) || algunaVerdad xs

-------------------------------------------------------------------------------------

pertenece :: Eq a => a -> [a] -> Bool
--Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual
--a e.

pertenece a [] = False
pertenece a (x:xs) = if a == x then True 
								else pertenece a xs

pertenece2 :: Eq a => a -> [a] -> Bool
pertenece2 e []	= False	
pertenece2 e (x:xs) = e == x || pertenece2 e xs 					
								
-------------------------------------------------------------------------
								
apariciones :: Eq a => a -> [a] -> Int
--Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.

apariciones e [] = 0
apariciones e (x:xs) =  if x == e then 1 + apariciones e xs		
									else apariciones e xs 

------------------------------------------------------

filtrarMenoresA :: Int -> [Int] -> [Int]
--Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
filtrarMenoresA n [] = []
filtrarMenoresA n (x:xs) = if x > n then x : filtrarMenoresA n xs
									else filtrarMenoresA n xs 


--------------------------------------------------------------------
filtrarElemento :: Eq a => a -> [a] -> [a]
--Dados un elemento y una lista filtra (elimina) todas las ocurrencias de ese elemento en la
--lista.

filtrarElemento n [] = []
filtrarElemento n (x:xs) = if not (n == x) then x: filtrarElemento n xs
											else filtrarElemento n xs 
											
------------------------------------------------------------------------------

mapLongitudes :: [[a]] -> [Int]

--Dada una lista de listas, devuelve la lista de sus longitudes. Aplique esta función a la lista
--de strings ["Estructuras", "de", "datos"] y observe el resultado.

mapLongitudes [] = []
mapLongitudes (x:xs) = longitud x : mapLongitudes xs 



-----------------------------------------------------------------------------

longitudMayorA :: Int -> [[a]] -> [[a]]
--Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
--de n elementos.
longitudMayorA n [] = []
longitudMayorA n (x:xs) = if longitud x > n 
						then x : longitudMayorA n xs
						else longitudMayorA n xs 
	
---------------------------------------------------------------------------

intercalar :: a -> [a] -> [a]
--Dado un elemento e y una lista xs, ubica a e entre medio de todos los elementos de xs.
intercalar a [] = []
intercalar a [b] = [b]
intercalar a (x:xs) = x : a : intercalar a xs 
						 	
----------------------------------------------------------------------------

snoc :: [a] -> a -> [a]
--Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la
--lista.
snoc [] a = [a]
snoc (x:xs) a  = x : snoc xs a  	

---------------------------------------------------------------------------------
append :: [a] -> [a] -> [a]
--Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
--elementos de la segunda a continuación. Definida en Haskell como ++.
append [] ys = ys
append (x:xs) ys = x : (append xs ys)

--------------------------------------------------------------------------------------
aplanar :: [[a]] -> [a]
--Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

-------------------------------------------------------------------------------------
reversa :: [a] -> [a]
--Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
--en Haskell como reverse.
reversa [] = []
reversa (x:xs) = reversa (xs) ++ [x]
-------------------------------------------------------------------------------------

zipMaximos :: [Int] -> [Int] -> [Int]
--Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
--máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
--las listas no necesariamente tienen la misma longitud.
zipMaximos [] ys = []
zipMaximos (x:xs) (y:ys)= if x > y
							then x : zipMaximos xs ys
							else y : zipMaximos xs ys

-------------------------------------------------------------------------------------------------------
zipSort :: [Int] -> [Int] -> [(Int, Int)]
--Dadas dos listas de enteros de igual longitud, devuelve una lista de pares (min, max), donde
--min y max son el mínimo y el máximo entre los elementos de ambas listas en la misma
--posición.
zipSort [] ys = []
zipSort (x:xs) (y:ys)=  (minimo x y , maximo x y) : zipSort xs ys 

----------------------------------------------------------------------------------------------

promedio :: [Int] -> Int
--Dada una lista de enteros, devuelve un número que es el promedio entre todos los elementos
--de la lista. ¿Pudo resolverla utilizando recursión estructural?

promedio (xs) = div (sumatoria xs)  (longitud xs)  
 
--------------------------------------------------------------------------
minimum' :: Ord a => [a] -> a
--Dada una lista devuelve el mínimo
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)
----------------------------------------------------------------------------


maximum' :: Ord a => [a] -> a
--Dada una lista devuelve el maximo.
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)



------------------Recursión sobre numeros
 
 
factorial :: Int -> Int
--Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
--llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial n = if n < 2 
				then 1
				else n * factorial (n-1)
				
-------------------------------------------------------------------------------------
---------------------------------------------------------------------------------


cuentaRegresiva :: Int -> [Int]
--Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
--n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
-- cuentaRegresiva n = [n]
cuentaRegresiva 1 = [1]
cuentaRegresiva n = if n > 1
				then  n : cuentaRegresiva (n-1)
			else []
	
-----------------------------------------------------------------------------------

contarHasta :: Int -> [Int]
--Dado un número n devuelve una lista cuyos elementos sean los números entre 1 y n (inclui-
--dos).
--contarHasta 1 = [1] 
contarHasta n =  if n > 1
				then  
				else contarHasta n
			
------------------------------------------------------------------------------------------

replicarN :: Int -> a -> [a]
--Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
replicarN 0 e = []
replicarN n e  = if n <= 0
				then replicarN n e
				else e : replicarN (n - 1) e

--------------------------------------------------------------------------------------
desdeHasta :: Int -> Int -> [Int]
--Dados dos números n y m devuelve una lista cuyos elementos sean los números entre n y m
--(incluidos).
desdeHasta n m = if n == m 
				then m : []
				else n : desdeHasta (n + 1) m 

------------------------------------------------------------------------------------

takeN :: Int -> [a] -> [a]
--Dados un número n y una lista xs, devuelve una lista con los primeros n elementos de xs.
--Si la lista posee menos de n elementos, se devuelve una lista vacía.
takeN 0 [] = []
takeN n (x:xs) = if n <= 0
			then takeN n xs
			else x : takeN (n - 1) xs 
				

--------------------------------------------------------------------------------

dropN :: Int -> [a] -> [a]
--Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
--recibida. Si xs posee menos de n elementos, se devuelve la lista completa.
dropN n []  = []
dropN n (x:xs) = if n == 0
				then x : dropN n xs
				else dropN n xs

-------------------------------------------------------------------------------------
splitN :: Int -> [a] -> ([a], [a])
--Dados un número n y una lista xs, devuelve un par donde la primera componente es la lista
--que resulta de aplicar takeN a xs, y la segunda componente el resultado de aplicar dropN
--a xs. ¿Conviene utilizar recursión?
splitN n xs = (takeN n xs, dropN n xs)
--------------------------------------------------------------------------------------------

-- splitMin :: Ord a => [a] -> (a, [a])
-- --Devuelve el mínimo y la lista sin él.
-- splitMin = (minimum 

