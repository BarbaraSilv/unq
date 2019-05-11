module StackSimple where

data Stack a = S [a] Int deriving (Show, Eq, Ord)
--Inv: int es el tamaño del stack

emptyStack :: Stack a
emptyStack = S [] 0

isEmptyStack :: Stack a -> Bool
isEmptyStack (S [] _) = True 
isEmptyStack (S _ _) = False 


push :: Ord a => a -> Stack a -> Stack a
push x (S xs n) = S (x:xs) n+1

pop :: Stack a -> Stack a
pop (S xs n) = S (tail xs) n-1

top :: Stack a -> a
top (S xs _) = head xs

lenStack :: Stack a -> Int --longitud
lenStack (S xs n) = n