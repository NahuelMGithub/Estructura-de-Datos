

import Map1

--emptyM :: Map k v --Propósito: devuelve un map vacío
--assocM :: Eq k => k -> v -> Map k v -> Map k v --Propósito: agrega una asociación clave-valor al map.
--lookupM :: Eq k => k -> Map k v -> Maybe v --Propósito: encuentra un valor dado una clave.
--deleteM :: Eq k => k -> Map k v -> Map k v --Propósito: borra una asociación dada una clave.
--domM :: Map k v -> [k] --Propósito: devuelve las claves del map.


valuesM :: Eq k => Map k v -> [Maybe v] --Propósito: obtiene los valores asociados a cada clave del map.
valuesM map = valoresDeClaves (domM map) map

valoresDeClaves :: Eq k => [k] ->  Map k v -> [Maybe v]
valoresDeClaves []     map  = [Nothing]
valoresDeClaves (k:ks) map  = lookupM k map : valoresDeClaves ks map  

todasAsociadas :: Eq k => [k] -> Map k v -> Bool --Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas []     map = True
todasAsociadas (k:ks) map = pertenece k (domM map) &&   todasAsociadas ks map

pertenece :: Eq a => a -> [a] -> Bool
pertenece y []     = False
pertenece y (x:xs) =  y == x  || pertenece y  xs


listToMap :: Eq k => [(k, v)] -> Map k v --Propósito: convierte una lista de pares clave valor en un map.
listToMap []            = emptyM
listToMap ((x,y):xyss)  = assocM x y (listToMap xyss)


mapToList :: Eq k => Map k v -> [(k, Maybe v)] --Propósito: convierte un map en una lista de pares clave valor.
-- el ejercicio pide  [(k, Maybe v)] pero si no pongo Maybe v me tira error
mapToList map = asociarClaveYValor (domM map) map

asociarClaveYValor :: Eq k => [k] -> Map k v -> [(k, Maybe v)]
asociarClaveYValor []     map = []
asociarClaveYValor (k:ks) map = ( k, (lookupM k map) ) : asociarClaveYValor ks map


--emptyM :: Map k v --Propósito: devuelve un map vacío
--assocM :: Eq k => k -> v -> Map k v -> Map k v --Propósito: agrega una asociación clave-valor al map.
--lookupM :: Eq k => k -> Map k v -> Maybe v --Propósito: encuentra un valor dado una clave.
--deleteM :: Eq k => k -> Map k v -> Map k v --Propósito: borra una asociación dada una clave.
--domM :: Map k v -> [k] --Propósito: devuelve las claves del map.



agruparEq :: Eq k => [(k, v)] -> Map k [v] --Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave
agruparEq kvs = listToMap (valoresAgrupados kvs)

valoresAgrupados :: Eq k => [(k, v)] -> [(k, [v])] 
valoresAgrupados []          = []
valoresAgrupados ((k,v):kvs) = agrupar (k,v) (valoresAgrupados kvs)
                           
agrupar :: Eq k => (k, v) -> [(k, [v])] -> [(k, [v])]
agrupar (k, v)  []           = [(k, [v])]
agrupar (k, v) ((k',vs):kvs) = if k == k'
                               then (k', (v :vs)) :   kvs
                               else (k', vs) :  agrupar (k, v) kvs


{- ME TIRA ERROR DE TIPO, 

 Type           : Maybe Int
 Does not match : Int
 
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
--cada número asociado con dichas claves.
incrementar cs map     = conValoresAumentados (domM map) cs map 

conValoresAumentados :: Eq k => [k] -> [k] -> Map k Int -> Map k Int
conValoresAumentados [] claves map     = emptyM
conValoresAumentados (k:ks) claves map = if  elem k claves
                                          then assocM k ((lookupM k map) +1) (conValoresAumentados ks claves map)
                                          else assocM k (lookupM k map) (conValoresAumentados ks claves map)





-}
7. mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
una clave del primero existe en el segundo, es reemplazada por la del primero
--}

--emptyM :: Map k v --Propósito: devuelve un map vacío
--assocM :: Eq k => k -> v -> Map k v -> Map k v --Propósito: agrega una asociación clave-valor al map.
--lookupM :: Eq k => k -> Map k v -> Maybe v --Propósito: encuentra un valor dado una clave.
--deleteM :: Eq k => k -> Map k v -> Map k v --Propósito: borra una asociación dada una clave.
--domM :: Map k v -> [k] --Propósito: devuelve las claves del map.



-- EJERCICIO NRO 5

{-
indexar :: [a] -> Map Int a
Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
su posición en la lista.
ocurrencias :: String -> Map Char Int
Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
en el string, y los valores la cantidad de veces que aparecen en el mismo.
Indicar los ordenes de complejidad en peor caso de cada función del usuario en base a la
implementación elegida, justicando las respuestas.


-}