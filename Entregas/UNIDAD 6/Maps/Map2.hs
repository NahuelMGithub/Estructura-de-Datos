-- 2. Como una lista de pares-clave valor con claves repetidas

module Map2 (Map, emptyM, assocM, lookupM, deleteM, domM)
    where

--Inv.Rep: No tiene

data Map   k v = M [(k, v) ] --   k de  Key y v de Value

emptyM :: Map k v --Propósito: devuelve un map vacío
assocM :: Eq k => k -> v -> Map k v -> Map k v --Propósito: agrega una asociación clave-valor al map.
lookupM :: Eq k => k -> Map k v -> Maybe v --Propósito: encuentra un valor dado una clave.
deleteM :: Eq k => k -> Map k v -> Map k v --Propósito: borra una asociación dada una clave.
domM :: Eq k => Map k v -> [k] --Propósito: devuelve las claves del map.

-- Costo: O(1)
emptyM  = M []

-- Costo: O(1)
assocM  k v  (M kvs) =  M ((k,v) : kvs)


-- Costo: O(n) donde n es la longitud de la lista-----> que pasa con los otros k? así doy solo el primero
lookupM k (M  kvs) = buscar k kvs

-- Costo: O(n) donde n es la longitud de la lista
buscar :: Eq k => k -> [(k, v)] -> Maybe v
buscar k  []             =  Nothing
buscar k  ((k', v'):kvs) =  if k == k'
                              then Just v'
                              else buscar k kvs

-- Costo: O(n) donde n es la longitud de la lista
deleteM k (M kvs) = M (borrarTodosLos k kvs)


-- Costo: O(n) donde n es la longitud de la lista
borrarTodosLos :: Eq k => k -> [(k,v)] ->  [(k,v)]
borrarTodosLos k    []            = []
borrarTodosLos k   ((k', v'):kvs) = if k == k'
                                     then borrarTodosLos k kvs
                                     else (k',v') : borrarTodosLos k kvs



-- Costo: O(n) donde n es la longitud de la lista
domM (M kvs) = claves kvs

-- Costo: O(n) donde n es la longitud de la lista
claves :: Eq k => [(k,v)] -> [k]
claves []          = []
claves ((k,v):kvs) =  agregarSiFalta k  (claves kvs) 


-- Costo: O(n) donde n es la longitud de la lista
agregarSiFalta :: Eq k => k -> [k] -> [k]
agregarSiFalta a xs = if elem a xs
                        then xs
                        else a : xs
