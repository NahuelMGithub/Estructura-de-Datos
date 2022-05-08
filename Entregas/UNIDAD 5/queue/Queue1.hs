-- ////////////////////////////// 3. Queue (cola) \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

--Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). Esto significa
--que los elementos salen en el orden con el que entraron, es decir, el que

module Queue1
    (Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue)
    where
data Queue a = Q [a]

emptyQ :: Queue a --Crea una cola vacía.
isEmptyQ :: Queue a -> Bool --Dada una cola indica si la cola está vacía.
queue :: a -> Queue a -> Queue a --Dados un elemento y una cola, agrega ese elemento a la cola.
firstQ :: Queue a -> a --Dada una cola devuelve el primer elemento de la cola.
dequeue :: Queue a -> Queue a --Dada una cola la devuelve sin su primer elemento.

--INV.REP: No posee.

-- Costo: O(1)
emptyQ = Q []

-- Costo: O(1)
isEmptyQ (Q xs) = null xs

-- Costo: O(1)
queue x (Q xs) = Q (x:xs)

-- Costo: O(1)
firstQ (Q xs) = last xs 

-- Costo: O(n) donde n es la longitud de la cola
dequeue (Q xs) = Q (sinPrimerElementoDeLaCola xs)

-- Costo: O(n) donde n es la longitud de la cola
sinPrimerElementoDeLaCola :: [a] -> [a] -- PC: La lista NO puede ser null
sinPrimerElementoDeLaCola (x:[])   = []
sinPrimerElementoDeLaCola (x:xs) = x : sinPrimerElementoDeLaCola xs 

