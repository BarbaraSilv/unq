module Stack where 

data Stack a = S [a] Int [a] deriving (Show, Eq, Ord) 
-- Inv. de representacion: Int es la longitud, [a] es lista de elementos ordenada de mayor a menor


stack1 :: Stack Int 
stack1 = S [1,2,3,4,5] 5 [5,4,3,2,1]

-- Si quiero tener un stack para pruebas, hacerlo asi
stack2 = push 2 (push 1 emptyStack)


--O(1)
emptyStack :: Stack a
emptyStack = S [] 0 []


--O(1)
isEmptyStack :: Stack a -> Bool
-- isEmptyStack (S []) = True 
-- isEmptyStack (S _) = False 
isEmptyStack (S xs _ _) = null xs



--O(n)
push :: Ord a => a -> Stack a -> Stack a
push x (S xs int lsMax) = (S (x:xs) (int+1) (actualizarMax x lsMax))

--O(n)
actualizarMax :: Ord a => a -> [a] -> [a]
actualizarMax e [] = [e]
actualizarMax e xs = if e > head xs 
    then e : xs
    else x : actualizarMax e xs



--O(n) -- tail O(1), sacarElem O(n)
pop :: Stack a -> Stack a
pop (S (xs) int lsMax) = S (tail xs) (int-1) (sacarElem (head xs) lsMax)

--O(n)
sacarElem :: a -> [a] -> [a]
sacarElem _ [] = []
sacarElem e (x:xs) = if e==x 
    then xs 
    else x : (sacarElem e xs)

--O(1)
top :: Stack a -> a
top (S xs _ _) = head xs


--Implementaremos este tipo abstracto de tal manera que la operación de máximo opere en tiempo constante. 
--Una forma de hacer esto es mantener mediante una lista adicional el maximo elemento conocido al momento de agregar 
--cada elemento en la pila. 
--Indicar los invariantes de representación correspondientes.

--O(1)
maxS :: Ord a => Stack a -> a
maxS (S _ _ lsMax) = head lsMax

-- O(1)
-- lenStack para poder hacer el de balanceado 
lenStack :: Stack a -> Int --longitud
lenStack (S _ n _) = n

