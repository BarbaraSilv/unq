-- TESORO

data Objeto = Chatarra | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

data Objeto = Cacharro | Tesoro deriving (Show, Eq)
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving (Show)

camino0 = Fin
camino1 = (Cofre [Tesoro] Fin )
camino2 = (Cofre [Cacharro] (Nada Fin) )
camino3 = (Cofre [Cacharro,Cacharro,Tesoro] Fin )
camino4 = (Cofre [Tesoro] (Nada (Nada (Fin))))
camino5 = (Nada (Nada (Fin)))
camino6 = (Nada (Nada ((Cofre [Cacharro,Cacharro,Tesoro] Fin ))))
camino7 = (Cofre [Cacharro,Cacharro] (Cofre [Cacharro,Cacharro](Cofre [Cacharro,Tesoro] (Nada(Nada Fin)))))
-- hayTesoro : Indica si hay un cofre con un tesoro en el camino
caminoLlenoDeCofres = Cofre [Cacharro] (Cofre [Tesoro] (Cofre [Tesoro] (Cofre [Tesoro, Tesoro,Tesoro] (Cofre [Tesoro] Fin))))


hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada cm)= hayTesoro cm
hayTesoro (Cofre xs cm) = cofreTieneTesoro xs || hayTesoro cm

cofreTieneTesoro :: [Objeto] -> Bool 
cofreTieneTesoro [] = False
cofreTieneTesoro (x:xs) = x == Tesoro || (cofreTieneTesoro xs)



--Camino = Fin | Cofre [obj] Camino | Nada Camino

hayTesoroEn :: Int -> Camino -> Bool
--hayTesoroEn : Indica si hay un tesoro en una cierta cantidad exacta de pasos. 
--Ej, si el numero de pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn _ Fin = False
hayTesoroEn 0 (Nada c) = False
hayTesoroEn 0 (Cofre obj _) = cofreTieneTesoro obj  
hayTesoroEn n (Nada c) = (hayTesoroEn (n-1) c) 
hayTesoroEn n (Cofre obj c) = (hayTesoroEn (n-1) c)




sumarTesoros :: [Objeto] -> Int
sumarTesoros [] = 0
sumarTesoros (x:xs) = if x == Tesoro then 1 + sumarTesoros xs else sumarTesoros xs

sumarTesorosEnCamino :: Camino -> Int
sumarTesorosEnCamino Fin = 0
sumarTesorosEnCamino (Nada c) = sumarTesorosEnCamino c
sumarTesorosEnCamino (Cofre obj c) = (sumarTesoros obj) + (sumarTesorosEnCamino c)




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


-- alMenosNTesoros : Indica si hay al menos “n” tesoros en el camino.
alMenosNTesoros :: Int -> Camino -> Bool 
alMenosNTesoros n c = (sumarTesorosEnCamino c) >= n



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