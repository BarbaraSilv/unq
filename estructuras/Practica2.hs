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

-- data types nuevos les tengo que poner deriving (Show) al final para poder usarlos en el repl/hugs

data Dir = Norte | Este | Sur | Oeste deriving (Show)

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = Norte


opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este



data Persona = Persona Nombre Int deriving (Show)
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
promedioEdad lsPersonas = sumarEdadesDePersonas lsPersonas `div` longitud lsPersonas

sumarEdadesDePersonas :: [Persona] -> Int
sumarEdadesDePersonas [] = 0
sumarEdadesDePersonas (x:xs) = sum (edad x : edades xs)

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
--

--data Pizza = Prepizza | Agregar Ingrediente Pizza deriving (Show) -- NO ENTIENDOOO <??>
-- Prepizza = [], Agregar = (:), Ingrediente = x, Pizza = xs
--type Prepizza = []

data Ingrediente = Salsa | Queso | Jamon | AceitunasVerdes Int deriving (Show)


--ingredientes :: Pizza -> [Ingrediente]


--tieneJamon :: Pizza -> Bool

--sacarJamon :: Pizza -> Pizza

--armarPizza :: [Ingrediente] -> Pizza

--duplicarAceitunas :: Pizza -> Pizza

--sacar :: [Ingrediente] -> Pizza -> Pizza

--cantJamon ::[Pizza] -> [(Int, Pizza)]

--mayorNAceitunas :: Int -> [Pizza] -> [Pizza]

--data Objeto = Cachorro | Tesoro deriving (Show)
--data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

--hayTesoro :: Camino -> Bool
--pasosHastaTesoro :: Camino -> Int 
--hayTesoroEn :: Int -> Camino -> Bool
--alMenosNTesoros :: Int -> Camino -> Bool
--cantTesorosEntre :: Int -> Int -> Camino -> Int

--data ListaNoVacia a = Unit a | Cons a (ListaNoVacia a) deriving (Show)
--length :: ListaNoVacia a -> Int --total
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




