

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
----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

----- E F I C I E N C I A A A A A A A A H H H H H / C O M P L E J I D A D

----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

-- (1) Medir la cantidad de recursos que usa el programa
-- Memoria
-- Espacio
-- Red/Banda ancha
-- Consumo de energia (bateria)
-- Procesador (tiempo) <------- BIG O





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
