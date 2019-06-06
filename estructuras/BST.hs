module BST where

data Tree a = Nil | Bin a (Tree a) (Tree a) deriving Show
-- data BST a = B (Tree a) deriving Show
--inv. rep: Tree es un BST
--inv. rep: -- Un arbol es un BST si:
-- Todos los elem. del subarbol izq. son todos elem. mas chicos que la raiz
-- Todos los elem. del subarbol der. son elem. mas grandes que la raiz
-- Ambos hijos son (recursivamente) un BST 


-- Todas tienen eficiencia O(log n)

isEmptyBST :: Tree a -> Bool 
isEmptyBST Nil = True 
isEmptyBST _ = False

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
--Prec: el arbol no es Nil
minBST (Bin x Nil Nil) = x
minBST (Bin x ti td) = minBST ti

maxBST :: Ord a => Tree a -> a
--Prec: el arbol no es Nil
maxBST (Bin x Nil Nil) = x
maxBST (Bin x ti td) = maxBST td

deleteMinBST :: Ord a => Tree a -> Tree a
deleteMinBST (Bin x Nil Nil) = Nil
deleteMinBST (Bin x ti td) = (Bin x (deleteMinBST ti) td)

deleteMaxBST :: Ord a => Tree a -> Tree a
deleteMaxBST (Bin x Nil Nil) = Nil
deleteMaxBST (Bin x ti td) = (Bin x ti (deleteMaxBST td))

-- <??> como se hace?
deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST e Nil = Nil 
deleteBST e (Bin x Nil Nil) = if e==x then Nil else (Bin x Nil Nil)
deleteBST e (Bin x ti td) = if e == x
    then removeRoot (Bin x ti td)
    else if e > x 
        then (Bin x ti (deleteBST e td)) 
        else (Bin x (deleteBST e ti) td)


removeRoot :: Tree a -> Tree a 
--Prec: not Nil
removeRoot (Bin x ti td) = if isEmptyBST td 
    then ti 
    else let (y,td') splitMinBST td in (Bin y ti td')



splitMinBST :: Ord a => Tree a -> (a, Tree a)
--Dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
splitMinBST tree = (minBST tree, deleteMinBST tree)

splitMaxBST :: Ord a => Tree a -> (a, Tree a)
--Dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
splitMaxBST tree = (maxBST tree, deleteMaxBST tree)


-- <??> Esta bien?
esBST :: Ord a => Tree a -> Bool
esBST Nil = True 
esBST (Bin x Nil Nil) = True
esBST (Bin x ti Nil) = x > (rootTree ti) && esBST ti
esBST (Bin x Nil td) = x < (rootTree td) && esBST td
esBST (Bin x ti td) = x > (rootTree ti) && x < (rootTree td) && esBST ti && esBST td

rootTree :: Tree a -> a 
--Prec: arbol no es Nil
rootTree (Bin x _ _) = x



-- <??> Esta bien?
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
--Dado un BST y un elemento, devuelve el máximo elemento que sea menor o igual al elemento dado.
--Ej 50 -> el numero mas grande menor o igual a 50 dentro del arbol, (40,48,49)->49, (10,20,30)->30
elMaximoMenorA e Nil = Nothing 
elMaximoMenorA e (Bin x Nil Nil) = Just x 
elMaximoMenorA e (Bin x ti td) = if e==x
    then Just x 
    else if e < x 
        then elMaximoMenorA e ti 
        else elMaximoMenorA e td



                -- 60
            -- 40       70
        -- 30    50   65   75



-- <??> Esta bien?
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
--Dado un BST y un elemento, devuelve el mínimo elemento que sea mayor o igual al elemento dado.
--Ej 50 -> el numero mas chico mayor o igual a 50 dentro del arbol, (50,70,90)->50, (61,82,104)->61
elMinimoMayorA e Nil = Nothing 
elMinimoMayorA e (Bin x Nil Nil) = Just x
elMinimoMayorA e (Bin x ti td) = if e==x
    then Just x 
    else if e < x 
        then elMinimoMayorA e ti 
        else elMinimoMayorA e td







