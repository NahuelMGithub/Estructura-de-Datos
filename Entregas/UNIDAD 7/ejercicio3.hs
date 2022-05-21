{-
Ejercicio 3
Dada la siguiente interfaz y costos para el tipo abstracto Map:
emptyM :: Map k v
Costo: O(1).
assocM :: Ord k => k -> v -> Map k v -> Map k v
Costo: O(log K).
lookupM :: Ord k => k -> Map k v -> Maybe v
Costo: O(log K).
deleteM :: Ord k => k -> Map k v -> Map k v
Costo: O(log K).
keys :: Map k v -> [k]
Costo: O(K).

recalcular el costo de las funciones como usuario de Map de la práctica anterior, siendo K es la
cantidad de claves del Map. Justicar las respuestas.

///////////////////// EJERCICIO ANTERIOR PERO CON COSTOS ACTUALIZADOS
1. valuesM :: Eq k => Map k v -> [Maybe v]
Propósito: obtiene los valores asociados a cada clave del map.
C(O)= n*logk  dado que por cada k, le ccuesta logk (lookupM)

2. todasAsociadas :: Eq k => [k] -> Map k v -> Bool
Propósito: indica si en el map se encuentran todas las claves dadas.
C(O)= n*logk  dado que por cada k, le ccuesta logk (lookupM) y la comparación es constante

3. listToMap :: Eq k => [(k, v)] -> Map k v
Propósito: convierte una lista de pares clave valor en un map.
C(O)= O(log K) * la longitud de la list [(k, v)] 

4. mapToList :: Eq k => Map k v -> [(k, v)]
Propósito: convierte un map en una lista de pares clave valor.
C(O)= n*logk  dado que por cada k, le ccuesta logk (lookupM)

5. agruparEq :: Eq k => [(k, v)] -> Map k [v]
Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
la misma clave.
C(O)= ?

6. incrementar :: Eq k => [k] -> Map k Int -> Map k Int
Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
cada número asociado con dichas claves.
C(O)= n*logk  dado que por cada k, le ccuesta logk (assocM)

7. mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
una clave del primero existe en el segundo, es reemplazada por la del primero.
C(O)= ?

-}