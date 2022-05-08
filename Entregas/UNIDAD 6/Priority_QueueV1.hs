
{--
emptyPQ :: PriorityQueue a
Propósito: devuelve una priority queue vacía.
isEmptyPQ :: PriorityQueue a -> Bool
Propósito: indica si la priority queue está vacía.
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
Propósito: inserta un elemento en la priority queue.
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
emptyPQ = (PQ [] )

--COSTO: (1)
isEmptyPQ (PQ xs)  = null xs


insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (PM xs ) = PM (colocarEnOrden x xs)


colocarEnOrden :: Ord a => a -> [a] -> [a]
colocarEnOrden x []     = [x]
colocarEnOrden x (y:ys) = if x < y
                            then x : ys
                            else y : (colocarEnOrden x ys)


findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (PQ xs )  = head xs


deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (PQ xs) = PQ (tail xs)