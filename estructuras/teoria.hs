

Tipos de Datos (algebraicos):
Bool, Int, [a], Tree a
-- Caracterizados por su tipo

Tipos ABSTRACTOS de Datos
--Caracterizados por su comportamiento. No me improta tanto su forma sino cómo interactuo con él.
-- Caracterizados por tu INTERFAZ

--Ej. maquina de café
-- Punto de vista del usuario -> comportamiento (y no conoce los mecanismos por detrás)
-- Punto de vista del implementador -> Mecanismos (y no le importa el usuario de la maquina)

-- INTERFAZ : lo que esta entre el usuario y el mecanismo. Permite al usuario comunicarse con él.
-- Tambien aprovechado por el implementador para realizar cambios sin tener que tocar mecanismo por mecanismo

-- INTERFAZ : para comunicar al usuario con la implementación

~~~~~~~~~~~~~~~
MAQUINA DO CAFÉ
~~~~~~~~~~~~~~~
Capacidad:
- 100 unidades de Agua
- 100 unidades de Café
- 100 unidades de Leche

Café solo -> 2 de Agua, 1 de Café
Café cortado -> 1 de Agua, 1 de Café, 1 de Leche

- se accede al cafe apretando un boton y metiendo una moneda. 
- el boton indica si la maquina tiene carga para realizar ese tipo de cafe.
- la maquina puede rellenarse cada tanto.
- queremos saber cuanto recauda la maquina.

-- Poner un nuevo Tipo Abstracto en un nuevo archivo disntinto (ej maquina-do-cafe.hs)

module MaquinaDeCafe where

data TipoCafe = Solo | Cortado


-- TODO ESTO ES LA INTERFAZ DE LA MAQUINA DO CAFÉ
nuevaMC :: MaquinaDeCafe --Una constante para describir a la maquina de cafe

disponibleMC :: MaquinaDeCafe -> TipoCafe -> Bool

recaudadoMC :: MaquinaDeCafe -> Int

pedirMC :: MaquinaDeCafe -> TipoCafe -> MaquinaDeCafe --Resultante despues de haber pedido el cafe
--Req. disponible el TipoCafe

reponerMC :: MaquinaDeCafe -> MaquinaDeCafe 
-- TODO ESTO ES LA INTERFAZ DE LA MAQUINA DO CAFÉ

-- Esta interfaz no me dice como esta programada internamente la MaquinaDeCafe, pero si me dice como puedo interactuar con ella


-- Entonces yo, como usuario de esta maquina, puedo hacer programas para interactuar con ella.

~~~~~~~~~~~~~
U S U A R I O --un archivo aparte
~~~~~~~~~~~~~

import MaquinaDeCafe 

cuantosCortados :: MaquinaDeCafe -> Int --Cuantos le puedo pedir a la maquina de cafe?
cuantosCortados m = if disponibleMC m 
    then 1 + cuantosCortados (pedirMC m)
    else 0 

totalRecaudacion :: [MaquinaDeCafe] -> Int 
totalRecaudacion [] = 0
totalRecaudacion (m:ms) = recaudadoMC m + totalRecaudacion ms

~~~~~~~~~~~~~~~~~~~~~~~~~
I M P L E M E N T A D O R --En el mismo archivo que la interfaz
~~~~~~~~~~~~~~~~~~~~~~~~~

type Money, Cafe, Awa, Leche = Int
data MaquinaDeCafe = MC Awa Cafe Leche Money

nuevaMC :: MaquinaDeCafe 
nuevaMC = MC 0 0 0 0

disponibleMC :: MaaquinaDeCafe -> TipoCafe -> Bool 
disponibleMC (MC a c l m) tipo = 
    awa tipo <= a && cafe tipo <= c && leche tipo <= l 

recaudadoMC :: MaquinaDeCafe -> Int
recaudadoMC (MC _ _ _ money) = money

pedirMC :: MaquinaDeCafe -> TipoCafe -> MaquinaDeCafe
pedirMC (MC a c l money) tipo = MC (a - awa tipo) (c - cafe tipo) (l - leche tipo) (money + 1)

reponerMC :: MaquinaDeCafe -> MaquinaDeCafe
reponerMC (MC a c l money) = MC 100 100 100 money

-- INVARIANTE:
-- Condiciones que tienen que cumplir los datos para que representen un valor valido
-- a c l money deben ser >= 0 
-- La implementacion de la interfaz debe ser consistente al constructor que elegi (y tambien el orden de los valores dentro del constructor)


----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------
-- ALGUNOS TIPOS ABSTRACTOS DE DATOS (T A D) (A D T)
----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

~~~~~~~~~~~~
Stack / Pila 
~~~~~~~~~~~~

module Stack where 

emptyS :: Stack a --Crea una pila nueva. O(1)
isEmptyS :: Stack a -> Bool --Me dice si esta vacia. O(1)
push :: a -> Stack a -> Stack a --Agrega elem. O(1)
pop :: Stack a -> Stack a --Prec: not isEmptyS. Saca un elem. O(1)
top :: Stack a -> a --Prec: not isEmptyS. --Devuelve el elemento arriba de todo (el ultimo) O(1)


data Stack a = Vacia | Meter a (Stack a)


~~~~~~~~~~~~
Queue / Cola
~~~~~~~~~~~~

module Queue where

emptyQ :: Queue a --Crea cola nueva. O(1)
isEmptyQ :: Queue a -> Bool -- O(1)
enqueue :: a -> Queue a -> Queue a -- O(n) 
dequeue :: Queue a -> Queue a --Prec: not isEmpty. O(1)
next :: Queue a -> a --Prec: not isEmpty. O(1)
-- Estas ultimas tres varia el O dependiendo como implementarlo, pero siempre uno es O(n) y los otros dos O(1)

data Queue a = Q [a]

~~~~~~~~~~~~~~~~
 Conjuntos / Set
~~~~~~~~~~~~~~~~

module Set where

data Set a = Set [a]

emptySet :: Set a --O(1)
addSet :: a -> Set a -> Set a --Si no esta, lo agrego. Necesito saber si elemento esta antes de agregarlo. O(n) 
elemSet :: a -> Set a -> Bool --O(n)
removeSet :: a -> Set a -> Set a  --O(n)
sizeSet :: Set a -> Int --O(n) 
unionSet :: Set a -> Set a -> Set a --O(n*n)?
intersectionSet :: Set a -> Set a -> Set a --O(n*n)?

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
---------------------------  O T R O - E J E M P L O
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Cómo medir eficiencia??

-- 1) Monitor / Profiling
-- Recursos de memoria y CPU 

-- 2) Tiempo (cuánto tarda) / Benchmark 
-- Comparando máquinas con diferentes recursos 

-- 3) Codigo Alto/Bajo
-- Cuantas operaciones a lo largo del tiempo

-- Vamos a medir eficiencia de codigo de alto nivel con Big O, cuántas operaciones con diferentes tamaños de párametro


-- 3 COSAS A TENER EN CUENTA AL IMPLEMENTAR ALGO:
--Eficiencia (La mejor: Caso 1 o Caso 3 (safa))
--Facilidad de uso/Mantenimiento (La mejor: Caso )
--Facilidad de implementacion (La mejor: Caso 2)

---////////////////////////// PRIMER CASO

data Color = Rojo | Verde | Azul | Negro 

data Celda = MkC Int Int Int Int 

-- constante, O(1)
celdaVacia :: Celda 
celdaVacia = MkC 0 0 0 0 

-- constante, O(1)
poner :: Color -> Celda -> Celda
poner Rojo (MkC n1 n2 n3 n4) = n1+1
poner Azul (MkC n1 n2 n3 n4) = n2+1
poner Verde (MkC n1 n2 n3 n4) = n3+1
poner Negro (MkC n1 n2 n3 n4) =  n4+1


-- constante, O(1)
sacar :: Color -> Celda -> Celda 
sacar Rojo (MkC n1 n2 n3 n4) = n1-1
sacar Azul (MkC n1 n2 n3 n4) = n2-1
sacar Verde (MkC n1 n2 n3 n4) = n3-1
sacar Negro (MkC n1 n2 n3 n4) =  n4-1

-- constante, O(1)
nroBolitas :: Color -> Celda -> Int 
nroBolitas Rojo (MkC n1 n2 n3 n4) = n1
nroBolitas Azul (MkC n1 n2 n3 n4) = n2
nroBolitas Verde (MkC n1 n2 n3 n4) = n3
nroBolitas Negro (MkC n1 n2 n3 n4) =  n4


-- lineal, O(n), n el tamaño del numero del parametro
ponerN :: Int -> Color -> Celda -> Celda 
ponerN n c celda = celda 
ponerN n c celda = poner c (ponerN (n-1) c celda) 


-- constante, O(1)
ponerN' :: Int -> Color -> Celda -> Celda 
ponerN' cant Rojo (MkC n1 n2 n3 n4) = (MkC (n1+cant) n2 n3 n4)
ponerN' cant Azul (MkC n1 n2 n3 n4) = (MkC n1 (n2+cant) n3 n4)
ponerN' cant Verde (MkC n1 n2 n3 n4) = (MkC n1 n2 (n3+cant) n4)
ponerN' cant Negro (MkC n1 n2 n3 n4) = (MkC n1 n2 n3 (n4+cant))



-- constante, O(1)
hayBolitas :: Color -> Celda -> Bool 
hayBolitas Rojo (MkC n _ _ _) = n > 0
hayBolitas Azul (MkC _ n _ _) = n > 0
hayBolitas Verde (MkC _ _ n _) = n > 0
hayBolitas Negro (MkC _ _ _ n) = n > 0


---////////////////////////// OTRA IMPLEMENTACION MÁS FACIL DE MANTENER (porque si quiero agregr 40 colores la anterior es una cagada)

data Celda = MkC [Color] deriving (Show, Eq)

-- O(1)
celdaVacia :: Celda
celdaVacia = MkC = [] 

-- O(n) n = el tamaño de xs (la lista)
nroBolitas :: Color -> Celda -> Int
nroBolitas c (MkC xs) = apariciones c xs 

-- O(1)
poner :: Color -> Celda -> Celda
poner c (MkC xs) = MkC (c:xs)

-- O(n)
sacar :: Color -> Celda -> Celda 
sacar c (MkC xs) = MkC (remove c xs)

-- O(n)
remove :: a -> [a] -> [a]
remove c [] = []
remove c (x:xs) =
    if c == x then xs else x: remove c xs 

--O(n), depnde de nroBolitas que depende de apariciones que es O(n)
hayBolitas :: Color -> Celda -> Bool 
hayBolitas c celda = nroBolitas c celda > 0



---////////////////////////// OTRA IMPLEMENTACION

data Celda = [(Color, Int)]

-- ESTO SE LLAMA LIST COMPREHENSION
-- O(1)
celdaVacia = MkC [ (c, 0) <- colores]


-- O(1)
nroBolitas :: Color -> Celda -> Int 
--Recorro la lista de colores, que es constante 

-- O(1)
poner :: Color -> Celda -> Celda
--Recorro la lista de colores, que es constante 

-- O(1)
sacar :: Color -> Celda -> Celda 
--Recorro la lista de colores, que es constante 

hayBolitas :: Color -> Celda -> Bool 



Privatizar Funciones // Crear Interfaz

module Celda1 (sacar,poner,hayBolitas,celdaVacia) -> En un archivo nuevo que importe estas, nomás puedo usar estas, todas las demás están ocultas

Que me permite hacer un modulo?
- Ocultar codigo 
- Ocultar implementacion (Caja Negra)
- Biblioteca (conjunto de funciones y tipos)
- Abstraccion 

El implementacion deberia decirme cuanto cuestan las funciones que están dentro de la caja negra 

Si dos modulos tienen la misma inerfaz pirdo cambiar la implementavion sin tener que tocar codigo ya implementado como usuario 
(Polimorfismo paramétrico) -> El polimorfismo acá 
(Polimorfismo ad-hoc : de ese solo objeto) -> El polimorfismo del paradigma de objetos

Tradeoff -> Cuando elijo una implementacion sobre otra (ej. sacrifico eficiencia por implementacion)


consejo: No pensar en eficiencia hasta que sea necesario, pensar primero en facilidad de implementacion

