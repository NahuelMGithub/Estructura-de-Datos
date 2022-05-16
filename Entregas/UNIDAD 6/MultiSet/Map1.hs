
-- Map1   Como una lista de pares-clave valor sin claves repetidas

module Map1 (Map, emptyM, assocM, lookupM, deleteM, domM)
    where

--Inv.Rep: en  M de  kvs, no hay claves repetidas en kvs

data Map  k v = M [(k,v)]  --   k de  Key y v de Value

emptyM :: Map k v --Propósito: devuelve un map vacío
assocM :: Eq k => k -> v -> Map k v -> Map k v --Propósito: agrega una asociación clave-valor al map.
lookupM :: Eq k => k -> Map k v -> Maybe v --Propósito: encuentra un valor dado una clave.
deleteM :: Eq k => k -> Map k v -> Map k v --Propósito: borra una asociación dada una clave.
domM :: Map k v -> [k] --Propósito: devuelve las claves del map. FIDEL le dice domM (dominio del map)

-- Costo: O(1)
emptyM  = M []

-- Costo: O(n) donde n es la longitud de la lista
assocM  k v  (M kvs) =  M (asociar k v kvs)

-- Costo: O(n) donde n es la longitud de la lista
asociar :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
asociar k v []             = [(k,v)]
asociar k v ((k', v'):kvs) = if k == k'
                               then (k', v) : kvs
                               else (k', v') :  asociar k v  kvs


-- Costo: O(n) donde n es la longitud de la lista
lookupM k (M  kvs) = buscar k kvs

-- Costo: O(n) donde n es la longitud de la lista
buscar :: Eq k => k -> [(k, v)] -> Maybe v
buscar k  []             =  Nothing
buscar k  ((k', v'):kvs) =  if k == k'
                              then Just v'
                              else buscar k kvs

-- Costo: O(n) donde n es la longitud de la lista
deleteM k (M kvs) = M (borrar k kvs)


-- Costo: O(n) donde n es la longitud de la lista
borrar :: Eq k => k -> [(k,v)] ->  [(k,v)]
borrar k    []           = []
borrar k   ((k', v'):kvs) = if k == k'
                            then kvs
                            else (k',v') : borrar k kvs

-- Costo: O(n) donde n es la longitud de la lista
domM (M kvs) = claves kvs

-- Costo: O(n) donde n es la longitud de la lista
claves :: [(k,v)] -> [k]
claves []          = []
claves ((k,v):kvs) = k : claves kvs