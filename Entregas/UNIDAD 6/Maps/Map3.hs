
--  3. Como dos listas, una de claves y otra de valores, donde la clave ubicada en la posición i está
--asociada al valor en la misma posición, pero de la otra lista.

module Map3 (emptyM, assocM, lookupM, deleteM, domM)
    where

--Inv.Rep: Ambas listas tienen la misma longitud, y el orden en el que ingresan los datos es igual

data Map  k v = M [k] [v]  --   k de  Key y v de Value

emptyM :: Map k v --Propósito: devuelve un map vacío
assocM :: Eq k => k -> v -> Map k v -> Map k v --Propósito: agrega una asociación clave-valor al map.
lookupM :: Eq k => k -> Map k v -> Maybe v --Propósito: encuentra un valor dado una clave.
deleteM :: Eq k => k -> Map k v -> Map k v --Propósito: borra una asociación dada una clave.
domM :: Map k v -> [k] --Propósito: devuelve las claves del map.


-- preguntar bien la diferencia del n vs N. Por algo que menciono en el teórico