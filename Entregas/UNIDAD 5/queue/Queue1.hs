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


emptyQ = Q []

isEmptyQ (Q xs) = esVacio xs

esVacio :: [a] -> Bool
esVacio []     = True
esVacio (x:xs) = False


queue x (Q xs) = Q (xs ++ [x])

firstQ (Q xs) = last xs 

dequeue (Q xs) = Q (sinPrimerElemento xs)

sinPrimerElemento :: [a] -> [a] -- PC: La lista NO puede ser null
sinPrimerElemento (x:[])   = []
sinPrimerElemento (x:xs) = sinPrimerElemento xs ++ [x] 