

data Contenedor = Comida | Oxigeno | Torpedo | Combustible  deriving (Eq,Show)	

data Componente = Escudo | CanonLaser | Lanzatorpedos | Motor Int | Almacen [Contenedor] deriving (Eq,Show)	


data Nave = Parte Componente Nave Nave | ParteBase deriving Show
--			NodeT  x         Ti   Td    Empty

data Tree a = EmptyT | NodeT a ( Tree a ) ( Tree a )  deriving Show


pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = (e == x) || (pertenece e xs)

interseccion :: Eq a => [a] -> [a] -> [a]
--Devuelve una lista que es la interseccion de dos listas
--Ej: [1,2,3,4] [3,4,5,6] --> [3,4]
interseccion [] _ = []
interseccion [a] _ = [a]
interseccion (x:xs) ys = if pertenece x ys then x : interseccion xs ys else interseccion xs ys

---Ejercicio 1
--nave1 :: Nave
--nave1 = Parte Escudo 
---			(Parte (Motor 2)
	--					(Parte (Almacen [Comida]) 
	--					ParteBase)
	--	    (Parte CañonLaser
--			            (Parte (Lanzatorpedos)
--			            ParteBase)
--		  

--Ejercicio 2
 --a

 
componentes :: Nave -> [Componente]
 --Retorna la lista de componente 
componentes ParteBase = []
componentes (Parte x ti td) = [x] ++ (componentes ti) ++ (componentes td) 


---------------------------------------------------------------------------------------------
--b
poderDePropulsion :: Nave -> Int
--Retorna el poder de propulsion de una nave. El poder de propulsion de una nave es la suma de los poderes de
--propulsion de los motores de la nave.
poderDePropulsion ParteBase = 0
poderDePropulsion (Parte x ti td) = numeroDeMotor x + (poderDePropulsion ti) + (poderDePropulsion td) 
-----------------------------------------------------------------------------------------
numeroDeMotor :: Componente -> Int
numeroDeMotor (Motor n) = n
numeroDeMotor _ = 0
---------------------------------------------------------------------------------------------------
--c

desarmarse :: Nave -> Nave
--Reemplaza armas por escudos.
desarmarse ParteBase = ParteBase
desarmarse (Parte x ti td )= if (x == CanonLaser) || (x == Lanzatorpedos) 
							then Parte Escudo (desarmarse ti) (desarmarse td)
							else Parte x (desarmarse ti) (desarmarse td)
-----------------------------------------------------------------------------
desarmarseNave :: Nave
desarmarseNave =(Parte Lanzatorpedos
					(Parte CanonLaser ParteBase ParteBase)
					(Parte (Motor 20) ParteBase ParteBase))
-------------------------------------------------------------------------------------

nave0 :: Nave
nave0 = ParteBase

nave1 :: Nave 
nave1 = Parte (Almacen [Comida, Comida, Oxigeno])
            (Parte Escudo
                (Parte Lanzatorpedos (ParteBase) (ParteBase))
                (ParteBase)
            )
            (Parte (Motor 5)
                (Parte (Almacen [Comida, Comida]) (ParteBase) (ParteBase))    
                (ParteBase)
            )


cantidadComida :: Nave -> Int
--Dada una nave devuelve la cantidad de comida. Cada aparicion de Comida vale 1
cantidadComida ParteBase = 0
cantidadComida (Parte (Almacen c) ti td )=  cantDeComidaEnAlmacen c + (cantidadComida ti) + (cantidadComida td) 
cantidadComida (Parte _ ti td) = (cantidadComida ti) + (cantidadComida td)

------------------------------------------------------------------------------------

cantDeComidaEnAlmacen ::  [Contenedor] -> Int 
cantDeComidaEnAlmacen [] = 0
cantDeComidaEnAlmacen (x:xs) = if x /= Comida 
							then cantDeComidaEnAlmacen xs
							else 1 + cantDeComidaEnAlmacen xs
--------------------------------------------------------------------------




cantidadComidaNave :: Nave
cantidadComidaNave = Parte Lanzatorpedos
						(Parte (Almacen [Combustible, Comida, Comida,Oxigeno]) 
								ParteBase 
								ParteBase)
						(Parte (Almacen [Torpedo, Comida, Oxigeno]) 
								ParteBase 
								ParteBase)





------------------------------------------------------------------------------------

naveToTree :: Nave -> Tree Componente
--Dada una nave la transforma en un  ́arbol de componentes
naveToTree ParteBase = EmptyT
naveToTree (Parte x ti td ) = NodeT x (naveToTree ti) (naveToTree td ) 

naveToTreeNave :: Nave
naveToTreeNave = (Parte Lanzatorpedos
						ParteBase	
						(Parte (Motor 20) ParteBase ParteBase))
--------------------------------------------------------------------------------------

aprovisionados :: [Contenedor] -> Nave -> Bool
--Dada una lista de contenedor chequea que cada almacen contenga todos esos tipos de contenedores.
--Precondicion: La nave tiene un almacen
-- aprovisionados [] Parte x ti td  = False
--aprovisionados xs (Parte (Almacen c) ti td) = (esAlmacenConProvisiones (Almacen c) xs) && (aprovisionados xs ti) && (aprovisionados xs td) 
aprovisionados xs ParteBase = True 
aprovisionados xs (Parte c ti td) = (esAlmacenConProvisiones c xs) && (aprovisionados xs ti) && (aprovisionados xs td)

-----------------------------------------------------------------------------------------------
esAlmacenConProvisiones :: Componente -> [Contenedor] -> Bool
--Dado un componente y una lista de contenedor chequea que sea un almacen y que contenga todos los tipos de contenedores 
--esAlmacenConProvisiones (Almacen []) xs = False  
-- esAlmacenConProvisiones _ [] = True
esAlmacenConProvisiones (Almacen ls) xs = contieneTodo xs ls
esAlmacenConProvisiones _ xs = True


-- El problema era que, en lugar de fijarse si pertenece el elemento de lsCont en el almacen, se estaba fijando
-- si pertenece el elemento del almacen en lsCont
-- Ej. ls = [Comida] con Almacen [Comida, Oxigeno, Maluma]. Se fijaba si los 3 del almacen estaban en ls, era al reves
contieneTodo :: [Contenedor] -> [Contenedor] -> Bool
contieneTodo [] _ = True 
contieneTodo (x:xs) ys = pertenece x ys  && (contieneTodo xs ys)  


-------------------------------------------------------------------------------------------------
pertenece2 :: Eq a => a -> [a] -> Bool
pertenece2 e []	= False	
pertenece2 e (x:xs) = e == x || pertenece2 e xs 


armasNivelN :: Int -> Nave -> [Componente]
--Devuelve las armas que haya en el nivel “n” de la nave.
armasNivelN 0 ParteBase = []
--armasNivelN 0 (Parte Lanzatorpedos ti td) = [Lanzatorpedos] 
--armasNivelN 0 (Parte CanonLaser ti td) = [CanonLaser] 
armasNivelN 0 (Parte x _ _) = concatenarArma x
--armasNivelN 0 (Parte _ ti td) = []
armasNivelN n (Parte _ ti td) = (armasNivelN (n-1) ti) ++ (armasNivelN (n-1) td )

concatenarArma :: Componente -> [Componente]
concatenarArma CanonLaser = [CanonLaser]
concatenarArma Lanzatorpedos = [Lanzatorpedos]
concatenarArma _ = []
