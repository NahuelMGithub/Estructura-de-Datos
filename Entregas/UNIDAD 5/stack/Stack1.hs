-- //////////////////////////////// 4 Stack (pila) \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

-- LIFO (last in, first out).

module Stack1 (Stack, emptyS, isEmptyS, push, top, pop, lenS)
    where

data Stack a = S [a]

emptyS :: Stack a --Crea una pila vacía.
isEmptyS :: Stack a -> Bool --Dada una pila indica si está vacía.
push :: a -> Stack a -> Stack a --Dados un elemento y una pila, agrega el elemento a la pila.
top :: Stack a -> a --Dada un pila devuelve el elemento del tope de la pila.
pop :: Stack a -> Stack a --Dada una pila devuelve la pila sin el primer elemento.
lenS :: Stack a -> Int --Dada una pila devuelve la longitud.

emptyS = S []

isEmptyS (S xs) = esVacio xs

esVacio :: [a] -> Bool
esVacio []     = True
esVacio (x:xs) = False

push x (S xs) = S (x:xs)

top (S xs) = head xs

pop (S xs) = S (tail xs)

lenS (S xs) = length xs
