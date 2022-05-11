
{--

valuesM :: Eq k => Map k v -> [Maybe v]
Propósito: obtiene los valores asociados a cada clave del map.
2. todasAsociadas :: Eq k => [k] -> Map k v -> Bool
Propósito: indica si en el map se encuentran todas las claves dadas.
3. listToMap :: Eq k => [(k, v)] -> Map k v
Propósito: convierte una lista de pares clave valor en un map.
4. mapToList :: Eq k => Map k v -> [(k, v)]
Propósito: convierte un map en una lista de pares clave valor.
5. agruparEq :: Eq k => [(k, v)] -> Map k [v]
Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
la misma clave.
6. incrementar :: Eq k => [k] -> Map k Int -> Map k Int
Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
cada número asociado con dichas claves.
7. mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
una clave del primero existe en el segundo, es reemplazada por la del primero.

--}

valuesM :: Eq k => Map k v -> [Maybe v]



emptyPQ :: PriorityQueue a -- Propósito: devuelve una priority queue vacía.
isEmptyPQ :: PriorityQueue a -> Bool --Propósito: indica si la priority queue está vacía.
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a --Propósito: inserta un elemento en la priority queue.
findMinPQ :: Ord a => PriorityQueue a -> a --Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a --Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.

