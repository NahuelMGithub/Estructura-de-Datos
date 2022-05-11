-- 2. Como una lista de pares-clave valor con claves repetidas

module Map2 (emptyM, assocM, lookupM, deleteM, domM)
    where

--Inv.Rep: No tiene

data Map   k v = M [(k, v) ] --   k de  Key y v de Value

emptyM :: Map k v --Propósito: devuelve un map vacío
assocM :: Eq k => k -> v -> Map k v -> Map k v --Propósito: agrega una asociación clave-valor al map.
lookupM :: Eq k => k -> Map k v -> Maybe v --Propósito: encuentra un valor dado una clave.
deleteM :: Eq k => k -> Map k v -> Map k v --Propósito: borra una asociación dada una clave.
domM :: Map k v -> [k] --Propósito: devuelve las claves del map.
