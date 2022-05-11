
{--


insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a

findMinPQ :: Ord a => PriorityQueue a -> a
Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
Precondición: parcial en caso de priority queue vacía.
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
Precondición: parcial en caso de priority queue vacía.
--}

module Priority_QueueV1
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ )
    where

data PriorityQueue a = PQ ([a])

-- INV. REP.: Los elementos se encuentran en Orden de Prioridad 

--COSTO: (1)
emptyPQ :: PriorityQueue a --Propósito: devuelve una priority queue vacía.
emptyPQ = (PQ [] )

--COSTO: (1)
isEmptyPQ :: PriorityQueue a -> Bool --Propósito: indica si la priority queue está vacía.
isEmptyPQ (PQ xs)  = null xs

--COSTO: (n*2) donde n es la longitud de la lista
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a --Propósito: inserta un elemento en la priority queue.
insertPQ x (PM xs ) = PM (colocarEnOrden x xs)

--COSTO: (n) donde n es la longitud de la lista
colocarEnOrden :: Ord a => a -> [a] -> [a]
colocarEnOrden x []     = [x]
colocarEnOrden x (y:ys) = if x < y
                            then x : ys
                            else y : (colocarEnOrden x ys)

--COSTO: (1)
findMinPQ :: Ord a => PriorityQueue a -> a -- Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
findMinPQ (PQ xs )  = head xs

--COSTO: (1)
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a --Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
deleteMinPQ (PQ xs) = PQ (tail xs)


-- ////////////////////////////////////////////////////////// EJERCICIO 2. 

--Implementar la función heapSort :: Ord a => [a] -> [a], que dada una lista la ordena de
--menor a mayor utilizando una Heap como estructura auxiliar. ¾Cuál es su costo?

-- No la hice porque NO SE LO QUE ES UN Heap PREGUNTAR!!!!