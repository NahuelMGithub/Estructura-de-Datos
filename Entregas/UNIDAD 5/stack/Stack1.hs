-- //////////////////////////////// 4 Stack (pila) \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

-- LIFO (last in, first out).

module Stack1 (Stack, emptyS, isEmptyS, push, top, pop, lenS)
    where

-- Inv.Rep. No posee


data Stack a = S [a]

emptyS :: Stack a --Crea una pila vacía.
isEmptyS :: Stack a -> Bool --Dada una pila indica si está vacía.
push :: a -> Stack a -> Stack a --Dados un elemento y una pila, agrega el elemento a la pila.
top :: Stack a -> a --Dada un pila devuelve el elemento del tope de la pila.
pop :: Stack a -> Stack a --Dada una pila devuelve la pila sin el primer elemento.
lenS :: Stack a -> Int --Dada una pila devuelve la longitud.

-- Costo: O(1) 
emptyS = S []

-- Costo: O(1) 
isEmptyS (S xs) = null xs

-- Costo: O(1) 
push x (S xs) = S (x:xs)

-- Costo: O(1) 
top (S xs) = head xs

-- Costo: O(n) donde n es la longitud de la lista
pop (S xs) = S (tail xs)

-- Costo: O(n) donde n es la longitud de la lista
lenS (S xs) = length xs
