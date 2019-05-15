
Bibliografia : Carmen Introduction to Algorythms
               Knuth  The Art of Computer Programming
(densos)

let (x,y) = (7,3) in x + y 
a x le doy el valor del primer elem. de la tupla, y a y le doy el valor del segundo elem. de la tupla



Tipos (de Datos) algebraicos:
Bool, Int, [a], Tree a, un registro
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
firstQ :: Queue a -> a --Prec: not isEmpty. O(1)
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

--module Celda1 (sacar,poner,hayBolitas,celdaVacia) -> En un archivo nuevo que importe estas, nomás puedo usar estas, todas las demás están ocultas

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

---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------

////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
// Binary Search Tree ( B S T ) // Arbol Binario de Busqueda
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

                    (5)
             (3)             (8)
        (2)     (4)             (9)
    (1)

data Set a = EmptyT | NodeT a (Set a) (Set a)
--Inv.: El Set es un BST.
--Invariante de representacion: Sin repetidos. Pero si ya tengo un BST, es una consecuencia automatica de la implementacion.
-- Redundante volver a decirlo, se puede obviar.

-- Un arbol es un BST si:
-- Todos los elem. del subarbol izq. son todos elem. mas chicos que la raiz
-- Todos los elem. del subarbol der. son elem. mas grandes que la raiz
-- Ambos hijos son (recursivamente) un BST 

--O(1)
emptyS :: Set a 
emptyS = EmptyT 

--O( altura del arbol ) = O(altura(BST)) = O(n)
addS :: a -> Set a -> Set a 
addS x EmptyT = (NodeT x (EmptyT) (EmptyT))
addS x (NodeT y ti td)) = if x == y 
    then (NodeT y ti td) 
    else if x < y 
        then NodeT y (addS x ti) td 
        else NodeT y ti (addS x td)

Que pasa si insertamos en el siguiente orden?: 5,2,1,4,8,3,7,9

                5
        2             8
    1     4        7    9
        3

Que pasa si insertamos en el siguiente orden?: 1,2,3,4,5,6,7,8,9

1
 2
  3
   4
    5
     6
      7
       8
        9

        (arbol desbalanceado) -> Mas dificil hacer operaciones en un arbol de altura 8

--O(n) = O(altura del arbol)
elemS :: x -> Set a -> Bool 
elemS x EmptyT = False 
elemS x (NodeT y ti td) = 
    x == y || (elemS x ti) || (elemS x td)
-- Esto funciona, peero hace mas trabajo del necesario a causa del Inv. del BST, que me ayudaria a buscar en nodos
-- más rápido (en lugar de tener que buscar TODOS los nodos).
    = x==y || if x < y then elemS x ti 
        else elemS x td 

-- O(altura del arbol)
removeS :: x -> Set a -> Set a 
removeS x EmptyT = EmptyT
removeS (NodeT y ti td) = 
    if x==y 
        then removeRoot (NodeT y ti td)
        else if x < y then (NodeT y (removeS x ti) td)
                      else (NodeT y ti (removeS x td))

-- Funcion privada; no forma parte de la Interfaz del Set, el usuario no la conoce, sólo usada x el implementador
-- O(altura del arbol)
removeRoot :: Set a -> Set a 
-- Prec: El BST No es vacío
removeRoot (NodeT y ti td) = 
    if isEmptyS td 
        then ti --Si el de la derecha esta vacio listo, engancho el izquierdo con el resto del BST 
        else let (y, td') = splitMinS td in (NodeT y ti td')   
        --Busco el elemento mas chico de todo el BST der. pero mas grande que el BST izq. para que ocupe el lugar del que voy a borrar.

--let (x,y) = (7,3) in x + y 
--a x le doy el valor del primer elem. de la tupla, y a y le doy el valor del segundo elem. de la tupla
        
-- O(altura del arbol)
splitMinS :: Set a -> Set a 
-- Separar el arbol en dos, por un lado el BST que resulta de sacar el minimo, por el otro el ¿minimo?
--Prec: El BST no está vacio
splitMinS (NodeT y ti td) = 
    if isEmptyS ti --Si no hay nodo a la izq. listo, el minimo es el primer elem. del BST
        then (y, td)
        else let (z, ti') = splitMinS ti in (z, NodeT y ti' td)


--O(n2) Concatenar dentro de una recursividad hace que sea n2
setToList :: Set a -> [a]
setToList EmptyT = []
setToList (NodeT a ti td) =
    setToList ti ++ [a] ++ setToList td
-- OJO!! Me desordena el BST

-- /////////////////////////////////////////////////////////////////////
-- /////////////////////// B A L A N C E A R - A R B O L
-- /////////////////////////////////////////////////////////////////////

Perfectamente Balanceado = un BST con todos los nodos ocupados

¿Altura de un arbol perf. balanceado?

# Nodos | Altura
///////////////// 
    1   | 0 (solo la raiz, 1)
    2   | NO EXISTE!! arbol balanceado (probablemente) peeero no es perfecto
    3   | 2
    7   | 3
    15  | 4

Cant. de nodos = 2^h - 1 |/| h = Altura BST --Arbol perf. balanceado

Altura BST = log(2) (n+1) |/| n = cant. de nodos

operaciones en un BST `pueden` llegar a ser -> O(log(2) n)



-- //////////////////////////////////////////////////////////////////7
-- ARBOL AVL (ANDELSON VERSKII Y LANDIS) -Alumbrado, Varrido y Limpieza
-- //////////////////////////////////////////////////////////////////7

cumple el inv. de BST pero ademas esta <balanceado>. 

para cada subarbol definimos el factor de balanceo como: altura (td) - altura (ti)

para que sea perf. balanceado, el factor de balanceo debe ser 0 

en un AVL, el invariante es que el factor de balanceo sea siempre -1, 0 o 1.

1
 2
  3 NO es un AVL valido 

OJO!! algoritmos de insercion y borrado de BST no funcionan para AVL porque no tienen en cuenta el nuevo invariante.

Implementacion de ADD y REMOVE de AVL son muuuuuuy mucho complicados 

/////////////////////////////////
// ADD EN AVL 
/////////////////////////////////

al insertar, pude haber arruinado a lo sumo el balanceo de todos los nodos padres hasta la raiz (pero solo esos).

una vez que inserte, me tengo que fijar si quedo bien balanceado el arbol. pero si no, lo tengo que rebalancear.

resulta util modificar el Datatype para que incluya la altura del arbol de cada nodo, para poder acceder a ella en O(1) 

data Set a = EmptyT | NodeT Int a (Set a) (Set a)
--Inv: Int representa la altura del arbol. 
--En lugar del Int, podría guardarme su Factor de Balanceo si quiero

Logica del algo. de ADD:

Si x < y, insertar recursivamente en ti, caso contrario en td. 

Despues verificar el factor de balanceo de la rama donde se inserto recursivamente.

// CASO 1 /////////////// 
el factor de balanceo era 0, despues de insertar me cambia a -1 o 1 o sigue en 0 (valido).

// CASO 2 ///////////////
el factor de balanceo era -1 o 1, despues de insertar pasa a 2 o -2 (invalido).

Supongamos que quedó en -2 (para resolver el caso 2, es exactamente la misma solucion y las mismas formulas).

                  x
         y              AVL-C
  AVL-A    AVL-B

// CASO 2.1 

El factor de y es -1

Solucion: Rotar el arbol 

                  y
        AVL-A              x 
                    AVL-B    AVL-C

// CASO 2.2 
el factor de balanceo de y es 0 = misma solucion del CASO 2.1 

// CASO 2.3 
el factor de balanceo de y es 1

                 x
           y               AVL-D 
    AVL-A     z(h+1) 
        AVL-B    AVL-C 

Solucion: doble rotacion. 

                   z 
         y                  x 
    AVL-A  AVL-B     AVL-C     AVL-D
    (h)  (h ó h+1)   (h)?       (h)

//////////////////////////////////////////////////////////////////
//  M A P
//////////////////////////////////////////////////////////////////


type Persona = ?
type Nombre = String
type DNI = Int

data Comunidad = C Tree (DNI, Persona) Tree (Nombre, Persona)
--Inv. rep: Los dos arboles son BST (y AVL)
--una persona esta representada en ambos arboles.
--Organizar la info. en pares me sirve para hacer mapeos (uno para la key, otro para el value).

--Aca ya se empieza a mezclar con BD, de la forma en la que puedo representar relaciones, nodos de grafos, etc.
--Ej. Tree (Persona, Persona), donde hay dos personas con alguna relacion (ej. amigoDe)


///////////////////////////////

data Map = M [(k, v)] deriving show (implementacion 1)
data Map = M [k] [v] deriving show (implementacion 2)
data Map = M (Tree (k,v)) deriving show (implementacion 3) (BST) 

// INTERFAZ

--O(n)
lookupM :: Eq k => [(k,v)] -> k -> Maybe v 
--Dado una lista de pares (key, value), le paso un key y me dice su value
lookUp [] _ = Nothing 
lookUp ((clave,valor):xs) key = if clave==key 
    then Just valor 
    else lookUp xs key 

-- Ej: lookupM 3 --> Persona3

Heap => BST pero con minTree en O(1)

/// COLAS DE PRIORIDAD / HEAP 

// Interfaz 


vaciaCP :: CP a 


escalar :: Ord a => a -> CP a -> CP a 


proxima :: Ord a => CP a -> a 
--Prec: tamaño > 0 


desencolar :: Ord a => CP a > CP a 


tamaño :: CP a -> Int 

// Implementacion 

data CP a => MkCP [a] Int deriving Show 

// Estructura De Heap 

data CP a = MkCP Int (Tree a) 

data Tree a = Nil | Bin  a (Tree a) (Tree a)

--Invariante: tiene dos partes:
1) Orden - el elemento mas chico esta en la raiz, el hijo izq. es (recursivamente) un Heap y el derecho también 

2) Balanceado - todos los niveles deben estar completos excepto quiza el ultimo 
 ademas el arbol es "izquierdista", osea, el ultimo nivel se completa de izq. a der. 

--Invariante: en MkCP n a, n es la cantidad de nodos del arbol



