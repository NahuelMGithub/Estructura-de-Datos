module MultiSet1 (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList )
    where

import Map1

data MultiSet a = MS (Map a Int)

{- 
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a (opcional)
Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
multiconjuntos tienen en común.
multiSetToList :: MultiSet a -> [(a, Int)]
Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
su cantidad de ocurrencias.
 -}

emptyMS :: MultiSet a 
emptyMS = MS emptyM
--Propósito: denota un multiconjunto vacío.

addMS :: Ord a => a -> MultiSet a -> MultiSet a
--Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.
addMS x (MS map) = if elem x (domM map)
                    then  MS( assocM x (fromJust (lookupM x map) +1) (deleteM x map) )
                    else  MS( assocM x 1 map)


fromJust (Just x) = x


ocurrencesMS :: Ord a => a -> MultiSet a -> Int
--Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese elemento en el multiconjunto.
ocurrencesMS x (MS map) = if elem x (domM map)
                          then fromJust (lookupM x map)
                          else 0

{-
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 
--Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de ambos multiconjuntos.
unionMS (MS map1) (MS map2) = MS (mapUnificado (domM map1) map1 map2)

mapUnificado :: Eq=> [k] -> Map k Int -> Map k Int -> Map k Int 
mapUnificado  [] m1 m2    = m2
mapUnificado (k:ks) m1 m2 = if elem k ks--(domM m2)
                             then m2
                             else m1 --assocM k (fromJust (lookupM k map)) (mapUnificado ks m1 m2)

-}

unionMS  = error "Hacer"
intersectionMS = error "Hacer"
multiSetToList = error "Hacer"

{-
emptyM :: Map k v --Propósito: devuelve un map vacío
assocM :: Eq k => k -> v -> Map k v -> Map k v --Propósito: agrega una asociación clave-valor al map.
lookupM :: Eq k => k -> Map k v -> Maybe v --Propósito: encuentra un valor dado una clave.
deleteM :: Eq k => k -> Map k v -> Map k v --Propósito: borra una asociación dada una clave.
domM :: Map k v -> [k] --Propósito: devuelve las claves del map. FIDEL le dice domM (dominio del map)
-}