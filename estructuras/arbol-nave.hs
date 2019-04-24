-- //////////////////////////////////////////////////
-- //////////////////////////////////////////////////
-- /////////////////////     NAVE
-- //////////////////////////////////////////////////
-- //////////////////////////////////////////////////

data Tree a = EmptyT | NodeT a ( Tree a ) (Tree a ) deriving (Show)

data Contenedor = Comida | Torpedo | Oxigeno | Combustible deriving (Show, Eq)

data Componente = Motor Int | CannonLaser | Escudo | Lanzatorpedos | Almacen [Contenedor] deriving Show

data Nave = ParteBase | Parte Componente Nave Nave deriving Show

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
--
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
--almacenTieneTodo (x:xs) (Almacen lsCont) = (almacenTiene x lsCont) && (almacenTieneTodo xs (Almacen lsCont))
-- <!!> OJO!!!! ME HABIA OLVIDADO DE PONER ALMACEN Y PUSE LSCONT.
almacenTieneTodo ls componente = True 

almacenTiene :: Contenedor -> [Contenedor] -> Bool 
almacenTiene x lsCont = elem x lsCont