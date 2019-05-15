module BST where

data Tree a = Nil | Bin a (Tree a) (Tree a) deriving Show
-- data BST a = B (Tree a) deriving Show
--inv. rep: Tree es un BST
--inv. rep: -- Un arbol es un BST si:
-- Todos los elem. del subarbol izq. son todos elem. mas chicos que la raiz
-- Todos los elem. del subarbol der. son elem. mas grandes que la raiz
-- Ambos hijos son (recursivamente) un BST 


-- Todas tienen eficiencia O(log n)

perteneceBST :: Ord a => a -> Tree a -> Bool
perteneceBST e Nil = False
perteneceBST e (Bin x ti td) = e==x || if e > x 
    then perteneceBST e td
    else perteneceBST e ti 

insertBST :: Ord a => a -> Tree a -> Tree a
insertBST e Nil = (Bin e (Nil) (Nil)) 
insertBST e (Bin x ti td) = if e > x
    then insertBST e td 
    else insertBST e ti 

minBST :: Ord a => Tree a -> a
minBST Nil =
minBST (Bin x ti td) = 


deleteMinBST :: Ord a => Tree a -> Tree a



maxBST :: Ord a => Tree a -> a



deleteMaxBST :: Ord a => Tree a -> Tree a



deleteBST :: Ord a => a -> Tree a -> Tree a



splitMinBST :: Ord a => Tree a -> (a, Tree a)



splitMaxBST :: Ord a => Tree a -> (a, Tree a)



esBST :: Ord a => Tree a -> Bool
esBST Nil = True 
esBST (Bin x ti td) =


elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a



elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a













