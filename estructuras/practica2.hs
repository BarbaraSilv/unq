


data Dir = Norte | Sur | Este | Oeste deriving Show


opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este
---------------------------------------------------------------------------------------

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste 
siguiente Oeste = Norte

---------------------------------------------------------------------------------------

data Persona = P Nombre Edad deriving Show

type Nombre = String
type Edad = Int

persona1 =(P "juan" 25)
persona2 =(P "gonza" 22)



nombre :: Persona -> String 
--Devuelve el nombre de una persona
nombre (P nm ed ) = nm

edad :: Persona -> Int 
--Devuelve la edad de una persona
edad (P nm ed) = ed

crecer :: Persona -> Persona
--Dada una persona la devuelve con su edad aumentada en 1.
crecer (P nm ed) = P nm (ed + 1)


cambioDeNombre :: Persona ->String -> Persona
--Dados un nombre y una persona, reemplaza el nombre de la persona por este otro.
cambioDeNombre (P nm ed) nuevonm = P nuevonm ed


esMenorQueLaOtra :: Persona -> Persona -> Bool
esMenorQueLaOtra x y = if edad x > edad y 
						then False
						else True 
-------------------------------------------------------
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra x y = edad x > edad y 
-------------------------------------------------------
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA e [] = []
mayoresA e(x:xs) = 
	if e <= edad x
		then x: mayoresA e xs
		else []
------------------------------------------------------------------
longitud :: [a] -> Int
longitud [] = 0
longitud (a:xs) = 1 + longitud xs

--------------------------------------------------------------
sumar :: Int -> Int -> Int
sumar x y = x + y
--------------------------------------------------------------
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

------------------------------------------------------------------
edades :: [Persona] -> [Int]
edades [] = []
edades (x:xs) = edad x : edades xs
-------------------------------------------------------------

promedioEdad :: [Persona] -> Int
--Dada una lista de personas devuelve el promedio de edad entre esas personas. La lista
--al menos posee una persona. 
promedioEdad [] = 0
promedioEdad xs = div (sumatoria (edades xs))  (longitud xs)

-----------------------------------------------------------------------------------
maximum' :: Ord a => [a] -> a
--Dada una lista devuelve el maximo.
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
------------------------------------------------------------

elMasViejo :: [Persona] -> Persona
--Dada una lista de personas devuelve la persona más vieja de la lista. La lista al menos
--posee una persona.
elMasViejo [x] = x 
elMasViejo (x:xs)  = if esMayorQueLaOtra x (elMasViejo xs)
					then x
					else elMasViejo xs	
------------------------------------------------------------------------------

--Definir los tipos de datos P okemon, como un T ipoDeP okemon (agua, fuego o planta) y
--un porcentaje de energía; y Entrenador, como un nombre y una lista de Pokemon. Luego
--definir las siguientes funciones:



data Pokemon = Pm TipoDePokemon Energia  deriving (Show)

type Energia = Int

data TipoDePokemon = Agua | Fuego | Planta deriving (Show,Eq)

data Entrenador = E NombreE Pokemons deriving (Show)   

type NombreE = String
type Pokemons =  [Pokemon]


pokemon1= (Pm Agua 10)
pokemon2=(Pm Fuego 10)
pokemon3=(Pm Planta 10)

entrenador1= (E "al" [pokemon1,pokemon2])

----------------------------------------------------------------------------------

leGana :: TipoDePokemon -> TipoDePokemon -> Bool
--Dados dos pokemon indica si el primero indudablemente le gana al segundo. Agua gana
--a fuego, fuego a planta y planta a agua. En cualquier otro caso no podemos afirmarlo.
leGana Agua Fuego = True
leGana Fuego  Planta = True
leGana Planta Agua = True
leGana _ _ = False

leGanaAlToque :: Pokemon -> Pokemon -> Bool 
leGanaAlToque (Pm t e) (Pm t2 e2) = leGana t t2

capturarPokemon :: Pokemon -> Entrenador -> Entrenador
--Agrega un pokemon a la lista de pokemon del entrenador.
capturarPokemon pokemon (E n pokemons) = E n (pokemon : pokemons)
------------------------------------------------------------------------

pokemonsDeEntrenador :: Entrenador -> [Pokemons]
pokemonsDeEntrenador (E n pokemons) = [pokemons] 

---------------------------------------------------------------------

cantidadDePokemons :: Entrenador -> Int
--Devuelve la cantidad de pokemons que posee el entrenador.
cantidadDePokemons entrenador = longitud (pokemonsDeEntrenador entrenador) 
-----------------------------------------------------------------------
tipo :: Pokemon -> TipoDePokemon
tipo (Pm tipoDePokemon energia) = tipoDePokemon


-----------------------------------------------------------------------

cantidadDePokemonsDeTipo :: TipoDePokemon -> Entrenador -> Int
--Devuelve la cantidad de pokemons de determinado tipo que posee el entrenador.
cantidadDePokemonsDeTipo tipoP (E n []) = 0
cantidadDePokemonsDeTipo tipoP (E n (x:xs)) = if tipoP == tipo (x)
										then 1 + cantidadDePokemonsDeTipo tipoP (E n xs)
										else cantidadDePokemonsDeTipo tipoP (E n xs)

-------------------------------------------------------------------------
------------------------
------------------------
{--
esExperto :: Entrenador -> Bool
--Dado un entrenador devuelve True si ese entrenador posee al menos un pokemon de
--cada tipo posible.
esExperto (E n xs) = pertenecePokemon (Agua xs) && 
					pertenecePokemon (Fuego xs) && 
					pertenecePokemon (Planta xs)
 --}
-----------------------------------------------------------------------------
pertenecePokemon :: TipoDePokemon -> [Pokemon] -> Bool
pertenecePokemon t [] = False
pertenecePokemon t (x:xs) = if t == tipo(x)
							then True
							else pertenecePokemon t xs 
-------------------------------------------------------------------------------
--pizza1 = Agregar [Queso, Salsa] Prepizza --MAL. Los ing. no pueden ir en una lista, si o si
-- tiene que ir ingrediente por ingrediente


data Pizza = Prepizza -- == []
		| Agregar Ingrediente Pizza  deriving Show
--			:    	x   		xs


-------------------------------------------		
data Ingrediente = Salsa
					| Queso
					| Jamon
					| AceitunasVerdes Int  deriving (Show,Eq)
-----------------------------------------------------------------------------------
pizza0 = Agregar Queso (Agregar Salsa Prepizza)
pizza1= Agregar Queso(Agregar Salsa (Agregar Jamon Prepizza ))
pizza2= Agregar Queso (Agregar Jamon (Agregar Jamon Prepizza))
-----------------------------------------------

ingredientes :: Pizza -> [Ingrediente]
--Dada una pizza devuelve la lista de ingredientes
ingredientes (Agregar ing p) = ing : ingredientes p 
----------------------------------------------------------------------------

tieneJamon :: Pizza -> Bool
--Dice si una pizza tiene jamón
tieneJamon Prepizza = False
tieneJamon (Agregar ing p) =  if ing /= Jamon 
							then tieneJamon p
							else True 
							 
--}
------------------------------------------------------------------------------
sacarJamon :: Pizza -> Pizza
--Le saca los ingredientes que sean jamón a la pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Agregar ing p ) = if ing /= Jamon
							then Agregar ing (sacarJamon p) -- x:(fun xs)
							else sacarJamon p --xs
-------------------------------------------------------------------------------
armarPizza :: [Ingrediente] -> Pizza
--Dada una lista de ingredientes construye una pizza
armarPizza []= Prepizza
armarPizza (x:xs) = Agregar x (armarPizza xs) 							
							
-----------------------------------------------------------------------------------				
duplicarAceitunas :: Pizza -> Pizza
--Recorre cada ingrediente y si es aceitunas duplica su cantidad						
duplicarAceitunas (Agregar (AceitunasVerdes n) p )= Agregar (AceitunasVerdes (n*2)) (duplicarAceitunas p)
duplicarAceitunas (Agregar ing p ) = Agregar ing (duplicarAceitunas p)  							
	

sacar :: [Ingrediente] -> Pizza -> Pizza
--Saca los ingredientes de la pizza que se encuentren en la lista
sacar [] (Agregar ing p)= Agregar ing p  
sacar (x:xs) (Agregar ing p) = if esIngredienteDePizza x (Agregar ing p)
								then sacarIngrediente x (Agregar ing p) 
								else sacar xs (Agregar ing p)
----------------------------------------------------------------------------------

esIngredienteDePizza :: Ingrediente -> Pizza -> Bool
esIngredienteDePizza i Prepizza = False
esIngredienteDePizza i (Agregar ing p ) = i == ing || esIngredienteDePizza i p

--------------------------------------------------------------------------------------

sacarIngrediente :: Ingrediente -> Pizza -> Pizza
sacarIngrediente i Prepizza = Prepizza
sacarIngrediente i (Agregar ing p ) = if i /= ing 
									then Agregar ing (sacarIngrediente i p) 
									else sacarIngrediente i p 

------------------------------------------------------------------------------------					
cantJamon :: [Pizza] -> [(Int, Pizza)]
--Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad
--de jamón de la pizza que es la segunda componente.							
cantJamon [] = []
cantJamon (pizza:pizzas) = (cantJamonEnPizza pizza, pizza) : cantJamon pizzas	
-------------------------------------------------------------------							
cantJamonEnPizza :: Pizza -> Int 
cantJamonEnPizza Prepizza = 0
cantJamonEnPizza (Agregar ing p) = 	if ing /= Jamon 
									then cantJamonEnPizza p
									else 1 + cantJamonEnPizza p 
---------------------------------------------------------------------------------							
mayorNAceitunas :: Int -> [Pizza] -> [Pizza]
--Devuelve las pizzas con más de “n” aceitunas.							
mayorNAceitunas 0 ls = ls
mayorNAceitunas _ [] = []
mayorNAceitunas n (x:xs)= if n < cantAceitunas x 
							then x : (mayorNAceitunas n xs )
							else mayorNAceitunas n xs 
---------------------------------------------------------------------------------------
cantAceitunas :: Pizza -> Int 
cantAceitunas Prepizza = 0
cantAceitunas (Agregar ing p )= numeroDeAceitunas ing + cantAceitunas p
----------------------------------------------------------------------------------------
numeroDeAceitunas :: Ingrediente -> Int
numeroDeAceitunas (AceitunasVerdes n ) = n 
numeroDeAceitunas _ = 0  							
							
------------------------------------------------------------------------------------------							



data Objeto = Cacharro | Tesoro 

data Camino = Fin | Cofre [Objeto] Camino | Nada Camino 

----------------------------------------------------
{--
hayTesoro :: Camino -> Bool
--Indica si hay un cofre con un tesoro en el camino
hayTesoro Camino 
hayTesoro 
--}

