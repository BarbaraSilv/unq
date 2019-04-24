module Practica2 where
import Practica1

-- #################################################################################
-- #################################################################################
-- #################################################################################
-- #################################################################################
-- #################################################################################
-- ##########################  P R A C T I C A #####################################
-- ################################ D O S ##########################################
-- #################################################################################
-- #################################################################################
-- #################################################################################
-- #################################################################################

-- TIPOS ALGEBRAICOS:
-- 1. ENUMERATIVOS (Ej. Dir)
-- 2. REGISTROS (Ej. Persona)
-- 3. RECURSIVOS (Ej. Pizza)

-- data types nuevos les tengo que poner deriving (Show) al final para poder usarlos en el repl/hugs

--data Dir = Norte | Este | Sur | Oeste deriving (Show)

-- siguiente :: Dir -> Dir
-- siguiente Norte = Este
-- siguiente Este = Sur
-- siguiente Sur = Oeste
-- siguiente Oeste = Norte


-- opuesto :: Dir -> Dir
-- opuesto Norte = Sur
-- opuesto Sur = Norte
-- opuesto Este = Oeste
-- opuesto Oeste = Este

persona1 = (Persona "Juan" 23)
persona2 = Persona "Hector" 89
persona3 = Persona "Gonza Crack" 22

data Persona = Persona Nombre Edad deriving (Show)
type Nombre  = String
type Edad    = Int
--data Persona = Persona { nombre :: String , edadPersona :: Int } deriving (Show)

nombre :: Persona -> Nombre
nombre (Persona name _ ) = name

edad :: Persona -> Int
edad (Persona _ age ) = age

edades :: [Persona] -> [Int]
edades [] = []
edades (x:xs) = edad x : edades xs

crecer :: Persona -> Persona --Devuelve persona con edad+1
crecer (Persona name age ) = Persona name (age + 1)

cambioDeNombre :: String -> Persona -> Persona -- Devuelve persona con el nombre String (cambiado)
cambioDeNombre nombreNuevo (Persona name age) = Persona nombreNuevo age

esMenorQueLaOtra :: Persona -> Persona -> Bool
esMenorQueLaOtra (Persona _ age1) (Persona _ age2) = age1 < age2

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA n ((Persona name age):lsPerson) = if age > n then (Persona name age):(mayoresA n lsPerson) else mayoresA n lsPerson  

promedioEdad :: [Persona] -> Int
promedioEdad lsPersonas = sumatoria (edades lsPersonas) `div` longitud lsPersonas

--sumarEdadesDePersonas :: [Persona] -> Int
--sumarEdadesDePersonas [] = 0
--sumarEdadesDePersonas (x:xs) = sum (edad x : edades xs)

elMasViejo :: [Persona] -> Persona
elMasViejo [x] = x 
elMasViejo (x:y:xs) = if esMenorQueLaOtra x y then elMasViejo (y:xs) else elMasViejo (x:xs) 


data Pokemon = Pokemon { tipoDePokemon ::  TipoDePokemon , porcentajeEnergia :: Int} deriving (Show)
data TipoDePokemon = Agua | Fuego | Planta deriving (Eq, Show) --No se si esta de mas el deriving aca

--data Entrenador = Entrenador { nombre :: String, lsPokemon :: [Pokemon] } deriving (Show)
data Entrenador = Entrenador Nombre [Pokemon] deriving (Show)
--No hace falta volver a poner type Nombre, ya lo defini en Persona!
--definir un type LsPokemon es redundante, [Pokemon] se entiende

leGanaAlToque :: Pokemon -> Pokemon -> Bool
-- Agua gana a fuego, fuego a planta y planta a agua. cualquier otro caso "no podemos afirmarlo"
leGanaAlToque (Pokemon tipo1 _) (Pokemon tipo2 _) =
-----  No puedo hacerlo asi, no tiene Tipo Eq. SALVOOOO que en deriving agregue el Eq, entonces si!!!
        (tipo1 == Agua && tipo2 == Fuego) 
        || (tipo1 == Fuego && tipo2 == Planta)
        || (tipo1 == Planta && tipo2 == Agua)

capturarPokemon :: Pokemon -> Entrenador -> Entrenador
capturarPokemon poke (Entrenador name lsPoke) = Entrenador name (poke:lsPoke) --No concatenaba xq le faltaban parentesis

cantidadDePokemons :: Entrenador -> Int
cantidadDePokemons (Entrenador _ lsPoke) = longitud lsPoke

cantidadDePokemonsDeTipo :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonsDeTipo _ (Entrenador _ []) = 0
cantidadDePokemonsDeTipo tipoPoke (Entrenador name ((Pokemon tipo energia):lsPoke)) = 
                if tipo == tipoPoke
                then 1 + cantidadDePokemonsDeTipo tipoPoke (Entrenador name lsPoke)
                else cantidadDePokemonsDeTipo tipoPoke (Entrenador name lsPoke)


esExperto :: Entrenador -> Bool
--True si tiene al menos un poke de cada tipo
esExperto trainer = (cantidadDePokemonsDeTipo Agua trainer) > 0 &&
                    (cantidadDePokemonsDeTipo Fuego trainer) > 0 &&
                    (cantidadDePokemonsDeTipo Planta trainer) > 0


data Pizza = Prepizza | Agregar Ingrediente Pizza deriving (Show) -- AHORA SI

-- Prepizza = [], Agregar = (:), Ingrediente = x, Pizza = xs
-- Me rompia la cabeza porque no es un Tipo Algebraico Enumerador, es un Tipo Algebraico Recursivo
pizza1 = Agregar Salsa (Agregar Queso (Agregar Jamon (Agregar (AceitunasVerdes 3) Prepizza)))
pizza2 = Agregar Jamon (Agregar Queso (Agregar Jamon (Agregar Jamon Prepizza)))
pizza3 = Prepizza

lspizzas :: [Pizza]
lspizzas = [pizza1,pizza2,pizza3]

data Ingrediente = Salsa | Queso | Jamon | AceitunasVerdes Int deriving (Show, Eq) 
ing1 = [Salsa, Salsa, Jamon, Queso]


ingredientes :: Pizza -> [Ingrediente]
ingredientes Prepizza = []
ingredientes (Agregar ing p ) =  ing : ingredientes p
--   Agregar=  (:) / ing = x / p = xs



tieneJamon :: Pizza -> Bool
tieneJamon Prepizza = False
tieneJamon pizza = pertenece Jamon (ingredientes pizza)

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Agregar ing p) = if ing /= Jamon then Agregar ing (sacarJamon p) else sacarJamon p

armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (i:ing) = Agregar i (armarPizza ing) 


duplicarAceitunas :: Pizza -> Pizza
-- Es mejor usar n*2 en lugar de n+n porque a veces haciendo n+n se pone reloco haskell
-- Antes de intentar solucionar el problema con "if", intentar solucionarlo haciendo Pattern Matching
-- En este caso, nomás en el caso en el que encuentro aceitunas me hace un cambio 
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Agregar (AceitunasVerdes n) p) = Agregar (AceitunasVerdes (n*2)) (duplicarAceitunas p)
duplicarAceitunas (Agregar x p) = Agregar x (duplicarAceitunas p)



sacar :: [Ingrediente] -> Pizza -> Pizza
--Saca los ingredientes de la pizza que se encuentren en la lista
sacar [] (Agregar ing p)= Agregar ing p  
sacar (x:xs) (Agregar ing p) = sacarIngrediente x (sacar xs p) 

sacarIngrediente :: Ingrediente -> Pizza -> Pizza
sacarIngrediente i Prepizza = Prepizza
sacarIngrediente i (Agregar ing p ) = if i /= ing 
			then Agregar ing (sacarIngrediente i p) 
			else sacarIngrediente i p 


cantJamon ::[Pizza] -> [(Int, Pizza)]
cantJamon [] = []
cantJamon (p:pizzas) = (apariciones Jamon (ingredientes p),p) : cantJamon pizzas


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



-- /////////////////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////////////////

-- Comento todo esto porque me esta dando problemas con el de la Practica 3

data Objeto = Cachorro | Tesoro deriving (Show, Eq)
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving (Show)
--                  Cofre Objeto/Nada = x, camino = xs = Recursion

-- Ejemplos

camino1 = Fin
camino2 = Cofre [Cachorro] (Nada (Cofre [Tesoro] (Nada Fin))) 
camino3 = Nada (Nada (Nada (Cofre [Tesoro] Fin)))
camino4 = Cofre [Cachorro] (Nada (Nada (Nada Fin))) 
camino5 = Nada (Nada (Cofre [Cachorro, Cachorro] (Nada (Cofre [Cachorro] (Cofre [Cachorro, Tesoro] Fin)))))
camino6 = Cofre [Tesoro,Cachorro] (Nada (Cofre [Cachorro, Tesoro, Tesoro] Fin))
camino7 = Nada (Cofre [Cachorro,Tesoro,Cachorro] Fin)

-- //////////////////////////


-- Si tengo multiples Tipos Algebraicos Recursivos en un mismo Datatype (Cofre camino, Nada camino) tengo que hacer
-- pattern matching por cada caso
hayTesoro :: Camino -> Bool
hayTesoro Fin = False 
hayTesoro (Nada c) = hayTesoro c -- Si no esta esto, se rompe con un camino que empieza con nada. Pero si lo tiene en el
-- medio al Nada camino, por que no se rompe?? 
hayTesoro (Cofre lsObj c) = hayTesoroEnCofre lsObj || (hayTesoro c)


-- No lo puedo resolver usando pertenece porque tendria que definir el Eq y es complicadoh
hayTesoroEnCofre :: [Objeto] -> Bool 
hayTesoroEnCofre [] = False
hayTesoroEnCofre (obj:lsObj) = (obj == Tesoro) || (hayTesoroEnCofre lsObj)



-- Precondicion: Hay Tesoro en el camino
-- Indica la cantidad de pasos hasta llegar al primer cofre con un tesoro
pasosHastaTesoro :: Camino -> Int 
--pasosHastaTesoro (Nada Fin) = 0 -- No hace falta xq tiene precondicion de que si o si va a encontrar un Tesoro
pasosHastaTesoro Fin = 0
pasosHastaTesoro (Nada cm) = 1 + pasosHastaTesoro cm 
pasosHastaTesoro (Cofre obj cm)= if hayTesoroEnLista obj then 0 else 1 + pasosHastaTesoro cm


hayTesoroEnLista :: [Objeto] -> Bool
hayTesoroEnLista [] = False
hayTesoroEnLista (x:xs) = x == Tesoro || hayTesoroEnLista xs 


-- True si pasosHastaTesoro == Int
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn _ Fin = False  
hayTesoroEn n c = pasosHastaTesoro c == n



-- True si hay al menos Int tesoros en el camino
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n camino = n == cantidadDeTesorosEnCamino camino


cantidadDeTesorosEnCamino :: Camino -> Int
-- cantidadDeTesorosEnCamino (Cofre [Tesoro] _) = 1
-- cantidadDeTesorosEnCamino (Cofre [_,Tesoro,_] _) = 1
-- cantidadDeTesorosEnCamino (Cofre [_,Tesoro] _) = 1
-- cantidadDeTesorosEnCamino (Cofre [Tesoro,_] _) = 1
cantidadDeTesorosEnCamino Fin = 0
--cantidadDeTesorosEnCamino (Nada Fin) = 0
cantidadDeTesorosEnCamino (Nada c) = cantidadDeTesorosEnCamino c
cantidadDeTesorosEnCamino (Cofre lsObj Fin) = cantidadDeTesoros lsObj
cantidadDeTesorosEnCamino (Cofre lsObj c) = cantidadDeTesoros lsObj + cantidadDeTesorosEnCamino c


cantidadDeTesoros :: [Objeto] -> Int
cantidadDeTesoros [] = 0
cantidadDeTesoros (obj:lsObj) = if obj == Tesoro then 1 + cantidadDeTesoros lsObj else cantidadDeTesoros lsObj

--siHayTesoroDevuelve1 :: [Objeto] -> Int
--siHayTesoroDevuelve1 lsObj = if hayTesoroEnCofre lsObj then 1 else 0
-- No me sirve porque que pasa si un cofre tiene mas de un tesoro?


caminoLlenoDeCofres = Cofre [Cachorro] (Cofre [Tesoro] (Cofre [Tesoro] (Cofre [Tesoro, Tesoro,Tesoro] (Cofre [Tesoro] Fin))))

sumarTesoros :: [Objeto] -> Int
sumarTesoros [] = 0
sumarTesoros (x:xs) = if x == Tesoro then 1 + sumarTesoros xs else sumarTesoros xs


cantTesorosEntre :: Int -> Int -> Camino -> Int
-- Devuelve cantidad de tesoros en el rango de un camino
-- Todos los caminos comienzan con 0
-- Ej. cantEntre 0 6 camino1
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


data ListaNoVacia a = Unit a | Cons a (ListaNoVacia a) deriving (Show)

lista1 = Unit 1
lista2 = Cons 1 (Cons 2 (Cons 3 (Unit 4)))

longitu :: ListaNoVacia a -> Int --total
longitu (Unit a) = 1
longitu (Cons n lista) = 1 + (longitu lista)


--head :: ListaNoVacia a -> a --parcial



--tail :: ListaNoVacia a -> ListaNoVacia a --parcial
--minimo :: ListaNoVacia Int -> Int --parcial

--data T a = A | B a | C a a | D (T a) | E a (T a) 
--Responder preguntas
--size :: T a -> Int
--sum :: T Int -> Int
--hayD :: T a -> Bool
--cantE :: T a -> Int
--recolectarC :: T a -> (a,a)
--toList :: T a -> [a]