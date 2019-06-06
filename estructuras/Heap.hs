import BST --(perteneceBST, insertBST, minBST, maxBST, deleteMinBST,  deleteMaxBST, deleteBST, splitMinBST)
-- (splitMaxBST, esBST, elMaximoMenorA, elMinimoMayorA)


data Heap = H Int ( Tree a ) deriving Show
-- Int es la cantidad de nodos en el Heap, Tree es un BST

-- 1) Orden - el elemento mas chico esta en la raiz, el hijo izq. es (recursivamente) un Heap y el 
--derecho también 

-- 2) Balanceado - todos los niveles deben estar completos excepto quiza el ultimo 
--ademas el arbol es "izquierdista", osea, el ultimo nivel se completa de izq. a der. 
       
-- El elemento de mayor prioridad es el que Yo defina 


-- MI heap va a ser = el elemento mayor es el de mayor prioridad


-- <??> Como hago para que, siendo un BST, el elemento mas chico este en la raiz??

emptyH :: Heap a
emptyH = H (0 Nil)

isEmptyH :: Heap a -> Bool
emptyH (H n tree) = isEmptyBST tree

insertH :: Ord a => a -> Heap a -> Heap a
insertH x (H n tree) = 


findMin :: Ord a => Heap a -> a -- Parcial en emptyH (Prec: not empty)
findMin (H n tree) x = 



deleteMin :: Ord a => Heap a -> Heap a -- Parcial en emptyH (Prec: not empty)
deleteMin 


splitMin :: Ord a => Heap a -> (a,Heap a) -- Parcial en emptyH (Prec: not empty)
splitMin (H n tree) = (minBST tree, (deleteBST (minBST tree) (H n tree)))
