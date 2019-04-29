module Stack where 

data Stack a = S [a] deriving Show 


stack1 :: Stack a 
stack1 = S [1,2,3,4,5]

--O(1)
emptyStack :: Stack a
emptyStack = S [] 


--O(1)
isEmptyStack :: Stack a -> Bool
isEmptyStack (Stack []) = True 
isEmptyStack (Stack _) = False 


--O(n)
push :: Ord a => a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)


--O(1)
pop :: Stack a -> Stack a
pop (Stack x:xs) = Stack xs


--O(1)
top :: Stack a -> a
top (Stack x:xs) = x


--O(n) --Esta implementacion no me vale. Deberia ser O(1)
maxS :: Ord a => Stack a -> a
maxS (Stack [x]) = x
maxS (Stack (x:xs)) = max x (maxS (Stack xs))



