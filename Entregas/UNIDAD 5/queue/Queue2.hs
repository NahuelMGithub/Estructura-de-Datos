-- ////////////////////////////// 3. Queue (cola) \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

--Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). Esto significa
--que los elementos salen en el orden con el que entraron, es decir, el que

module Queue2
    (Queue, emptyQ, isEmptyQ, queue, firstQ, dequeue)
    where
data Queue a = Q [a]

emptyQ :: Queue a --Crea una cola vacía.
isEmptyQ :: Queue a -> Bool --Dada una cola indica si la cola está vacía.
queue :: a -> Queue a -> Queue a --Dados un elemento y una cola, agrega ese elemento a la cola.
firstQ :: Queue a -> a --Dada una cola devuelve el primer elemento de la cola.
dequeue :: Queue a -> Queue a --Dada una cola la devuelve sin su primer elemento.

--INV.REP: No posee.

--2. Implemente ahora la versión que agrega por delante y quita por el final de la lista. Compare
-- la eficiencia entre ambas implementaciones

-- Costo: O(1)
emptyQ = Q []

-- Costo: O(1)
isEmptyQ (Q xs) = null xs

-- Costo: O(n) siendo n, la longitud de xs
queue x (Q xs) = Q (xs ++ [x])

-- Costo: O(1)
firstQ (Q xs) = head xs 

-- Costo: O(1) 
dequeue (Q xs) = Q (tail xs)



-- ES MAS ECONÓMICO en esta forma quitar elementos, pero es mas costos agregarlos. Inverso a Queue1

